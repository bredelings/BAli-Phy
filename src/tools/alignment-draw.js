// Defines the dependency-free scale API and initializes browser alignment viewers.
(function (globalScope) {
'use strict';

// Five representative stops from the perceptually uniform, CC0 viridis palette.
const PROPERTY_PALETTE = [
    [68, 1, 84],
    [59, 82, 139],
    [33, 145, 140],
    [94, 201, 98],
    [253, 231, 37],
];

const AUTO_SCALE_MIN_VALUES = 20;
const AUTO_SCALE_MIN_DISTINCT_POSITIVE = 8;
const AUTO_SCALE_BINS = 16;
const AUTO_SCALE_ENTROPY_MARGIN = 0.10;

// Restricts a number to the inclusive interval [lower, upper].
function clamp(value, lower, upper)
{
    return Math.min(upper, Math.max(lower, value));
}

// Returns the finite numeric observations used to define a property scale.
function finiteValues(values)
{
    if (!Array.isArray(values))
        throw new TypeError('Scale values must be an array.');
    const finite = values.filter((value) => Number.isFinite(value));
    if (finite.length === 0)
        throw new RangeError('A property scale needs at least one finite value.');
    return finite;
}

// Finds numeric bounds without expanding a potentially large array into arguments.
function extent(values)
{
    let lower = Infinity;
    let upper = -Infinity;
    for (const value of values) {
        lower = Math.min(lower, value);
        upper = Math.max(upper, value);
    }
    return {lower, upper};
}

// Computes a linearly interpolated quantile from an already sorted array.
function quantile(sorted, probability)
{
    if (sorted.length === 1)
        return sorted[0];
    const position = clamp(probability, 0, 1) * (sorted.length - 1);
    const lowerIndex = Math.floor(position);
    const upperIndex = Math.ceil(position);
    const fraction = position - lowerIndex;
    return sorted[lowerIndex] * (1 - fraction) + sorted[upperIndex] * fraction;
}

// Returns full or percentile-clipped bounds in the selected transform's domain.
function automaticScaleBounds(observations, transform, range = 'robust')
{
    const values = finiteValues(observations);
    if (!['linear', 'log10'].includes(transform))
        throw new RangeError(`Automatic bounds do not support ${transform}.`);
    if (!['robust', 'full'].includes(range))
        throw new RangeError(`Unknown automatic range: ${range}`);
    if (transform === 'log10' && values.some((value) => value < 0))
        throw new RangeError('A log10 scale cannot display negative values.');
    const eligible = transform === 'log10' ?
        values.filter((value) => value > 0) : values;
    if (eligible.length === 0)
        throw new RangeError('A log10 scale needs at least one positive value.');
    const sorted = [...eligible].sort((left, right) => left - right);
    if (range === 'robust')
        return {lower: quantile(sorted, 0.02), upper: quantile(sorted, 0.98)};
    return {lower: sorted[0], upper: sorted[sorted.length - 1]};
}

// Creates evenly positioned legend ticks in transformed scale space.
function continuousLegendTicks(count, lower, upper, transform, inverse)
{
    if (!Number.isInteger(count) || count < 2)
        throw new RangeError('A legend needs at least two ticks.');
    if (lower === upper)
        return [{position: 0.5, value: lower}];
    const transformedLower = transform(lower);
    const transformedUpper = transform(upper);
    // Converts each evenly spaced transformed coordinate back to raw units.
    return Array.from({length: count}, (_, index) => {
        const position = index / (count - 1);
        if (index === 0)
            return {position, value: lower};
        if (index === count - 1)
            return {position, value: upper};
        const transformed = transformedLower + position * (transformedUpper - transformedLower);
        return {position, value: inverse(transformed)};
    });
}

// Builds a clipped continuous normalizer for either linear or logarithmic data.
function createContinuousScale(values, options, transform, inverse)
{
    const observed = extent(values);
    const lower = options.lower === undefined ? observed.lower : options.lower;
    const upper = options.upper === undefined ? observed.upper : options.upper;
    if (!Number.isFinite(lower) || !Number.isFinite(upper))
        throw new RangeError('Scale bounds must be finite.');
    if (lower > upper)
        throw new RangeError('The lower scale bound must not exceed the upper bound.');

    const transformedLower = transform(lower);
    const transformedUpper = transform(upper);

    // Maps a raw property value to a clipped palette coordinate.
    function normalize(value)
    {
        if (!Number.isFinite(value))
            return null;
        if (lower === upper) {
            if (value < lower)
                return 0;
            if (value > upper)
                return 1;
            return 0.5;
        }
        const transformed = transform(value);
        return clamp((transformed - transformedLower) /
                     (transformedUpper - transformedLower), 0, 1);
    }

    // Returns raw values and normalized positions for a numeric legend.
    function legendTicks(count)
    {
        return continuousLegendTicks(count, lower, upper, transform, inverse);
    }

    return {normalize, legendTicks, lower, upper, transform: options.transform};
}

// Locates an arbitrary value among sorted distinct rank groups.
function interpolatedRank(groups, value)
{
    if (value <= groups[0].value)
        return 0;
    if (value >= groups[groups.length - 1].value)
        return 1;
    let left = 0;
    let right = groups.length - 1;
    while (left + 1 < right) {
        const middle = Math.floor((left + right) / 2);
        if (value === groups[middle].value)
            return groups[middle].position;
        if (value < groups[middle].value)
            right = middle;
        else
            left = middle;
    }
    if (value === groups[right].value)
        return groups[right].position;
    const fraction = (value - groups[left].value) /
                     (groups[right].value - groups[left].value);
    return groups[left].position +
           fraction * (groups[right].position - groups[left].position);
}

// Inverts the piecewise-linear tied-rank map for consistent legend labels.
function valueAtRank(groups, position)
{
    if (position <= groups[0].position)
        return groups[0].value;
    if (position >= groups[groups.length - 1].position)
        return groups[groups.length - 1].value;
    let left = 0;
    let right = groups.length - 1;
    while (left + 1 < right) {
        const middle = Math.floor((left + right) / 2);
        if (position === groups[middle].position)
            return groups[middle].value;
        if (position < groups[middle].position)
            right = middle;
        else
            left = middle;
    }
    const fraction = (position - groups[left].position) /
                     (groups[right].position - groups[left].position);
    return groups[left].value +
           fraction * (groups[right].value - groups[left].value);
}

// Builds an empirical-rank normalizer, assigning tied observations their midrank.
function createRankScale(values)
{
    const sorted = [...values].sort((left, right) => left - right);
    if (sorted[0] === sorted[sorted.length - 1]) {
        const constant = sorted[0];
        return {
            normalize: (value) => !Number.isFinite(value) ? null :
                value < constant ? 0 : value > constant ? 1 : 0.5,
            legendTicks: () => [{position: 0.5, value: constant}],
            lower: constant,
            upper: constant,
            transform: 'rank',
        };
    }

    const groups = [];
    let first = 0;
    while (first < sorted.length) {
        let last = first;
        while (last + 1 < sorted.length && sorted[last + 1] === sorted[first])
            last += 1;
        let position = ((first + last) / 2) / (sorted.length - 1);
        if (first === 0)
            position = 0;
        if (last === sorted.length - 1)
            position = 1;
        groups.push({value: sorted[first], position});
        first = last + 1;
    }

    // Maps a property value to its empirical percentile, interpolating between groups.
    function normalize(value)
    {
        if (!Number.isFinite(value))
            return null;
        return interpolatedRank(groups, value);
    }

    // Returns empirical quantiles at evenly spaced legend positions.
    function legendTicks(count)
    {
        if (!Number.isInteger(count) || count < 2)
            throw new RangeError('A legend needs at least two ticks.');
        // Inverts the displayed tied-rank map at evenly spaced palette positions.
        return Array.from({length: count}, (_, index) => {
            const position = index / (count - 1);
            return {position, value: valueAtRank(groups, position)};
        });
    }

    return {
        normalize,
        legendTicks,
        lower: sorted[0],
        upper: sorted[sorted.length - 1],
        transform: 'rank',
    };
}

// Creates a reusable linear, log10, or empirical-rank property scale.
function createScale(observations, requestedOptions = {})
{
    const values = finiteValues(observations);
    const options = {...requestedOptions};
    options.transform = options.transform || 'linear';

    if (options.transform === 'linear')
        return createContinuousScale(values, options, (value) => value, (value) => value);
    if (options.transform === 'log10') {
        if (values.some((value) => value < 0))
            throw new RangeError('A log10 scale cannot display negative values.');
        const positive = values.filter((value) => value > 0);
        if (positive.length === 0)
            throw new RangeError('A log10 scale needs at least one positive value.');
        const observed = extent(positive);
        const lower = options.lower === undefined ? observed.lower : options.lower;
        const upper = options.upper === undefined ? observed.upper : options.upper;
        if (lower <= 0 || upper <= 0)
            throw new RangeError('A log10 scale requires positive bounds.');
        return createContinuousScale(
            values, {...options, lower, upper}, Math.log10, (value) => 10 ** value);
    }
    if (options.transform === 'rank')
        return createRankScale(values);
    throw new RangeError(`Unknown property transform: ${options.transform}`);
}

// Reports whether a property's semantics and observed domain permit logarithmic display.
function canUseLogScale(propertyName, observations)
{
    if (!Array.isArray(observations))
        throw new TypeError('Scale values must be an array.');
    const values = observations.filter((value) => Number.isFinite(value));
    // NOTE: Remove this built-in-name policy once property results carry scale metadata.
    if (propertyName === 'posSelection' || values.length === 0)
        return false;
    return values.some((value) => value > 0) &&
           values.every((value) => value >= 0);
}

// Measures how evenly normalized observations occupy a fixed number of color bins.
function normalizedColorEntropy(values, scale, binCount = AUTO_SCALE_BINS)
{
    const counts = Array(binCount).fill(0);
    for (const value of values) {
        const position = scale.normalize(value);
        const bin = Math.min(binCount - 1, Math.floor(position * binCount));
        counts[bin] += 1;
    }
    let entropy = 0;
    for (const count of counts) {
        if (count === 0)
            continue;
        const probability = count / values.length;
        entropy -= probability * Math.log(probability);
    }
    return entropy / Math.log(binCount);
}

// Selects log10 only when sufficient positive data make materially better use of color.
function preferredTransform(propertyName, observations)
{
    if (!Array.isArray(observations))
        throw new TypeError('Scale values must be an array.');
    const values = observations.filter((value) => Number.isFinite(value));
    if (!canUseLogScale(propertyName, values) || values.length < AUTO_SCALE_MIN_VALUES)
        return 'linear';
    const positive = values.filter((value) => value > 0);
    if (new Set(positive).size < AUTO_SCALE_MIN_DISTINCT_POSITIVE)
        return 'linear';
    const linearBounds = automaticScaleBounds(values, 'linear', 'robust');
    const logBounds = automaticScaleBounds(values, 'log10', 'robust');
    if (linearBounds.lower === linearBounds.upper || logBounds.lower === logBounds.upper)
        return 'linear';
    const linear = createScale(values, {transform: 'linear', ...linearBounds});
    const logarithmic = createScale(values, {transform: 'log10', ...logBounds});
    const linearEntropy = normalizedColorEntropy(positive, linear);
    const logEntropy = normalizedColorEntropy(positive, logarithmic);
    return logEntropy >= linearEntropy + AUTO_SCALE_ENTROPY_MARGIN ? 'log10' : 'linear';
}

// Fades an RGB property color toward white according to AU certainty.
function blendWithWhite(color, certainty)
{
    if (!Array.isArray(color) || color.length !== 3 ||
        color.some((channel) => !Number.isFinite(channel)))
        throw new TypeError('A color must contain three finite RGB channels.');
    if (!Number.isFinite(certainty))
        throw new TypeError('Alignment certainty must be finite.');
    const weight = clamp(certainty, 0, 1);
    return color.map((channel) => Math.round(255 + (channel - 255) * weight));
}

// Interpolates the fixed sequential palette at a normalized coordinate.
function paletteColor(position)
{
    const scaled = clamp(position, 0, 1) * (PROPERTY_PALETTE.length - 1);
    const lower = Math.floor(scaled);
    const upper = Math.ceil(scaled);
    const fraction = scaled - lower;
    return PROPERTY_PALETTE[lower].map((channel, index) =>
        Math.round(channel * (1 - fraction) + PROPERTY_PALETTE[upper][index] * fraction));
}

// Converts an sRGB channel to the linear-light value used for contrast ratios.
function linearLight(channel)
{
    const value = clamp(channel, 0, 255) / 255;
    if (value <= 0.04045)
        return value / 12.92;
    return ((value + 0.055) / 1.055) ** 2.4;
}

// Computes WCAG relative luminance for an RGB color.
function relativeLuminance(color)
{
    return 0.2126 * linearLight(color[0]) +
           0.7152 * linearLight(color[1]) +
           0.0722 * linearLight(color[2]);
}

// Chooses black or white text according to which has the higher contrast ratio.
function contrastingTextColor(background)
{
    const luminance = relativeLuminance(background);
    const blackContrast = (luminance + 0.05) / 0.05;
    const whiteContrast = 1.05 / (luminance + 0.05);
    return blackContrast >= whiteContrast ? [0, 0, 0] : [255, 255, 255];
}

// Formats an RGB triplet for a cell's inline background or foreground.
function rgb(color)
{
    return `rgb(${color[0]}, ${color[1]}, ${color[2]})`;
}

// Produces the CSS gradient shared by one- and two-dimensional legends.
function paletteGradient()
{
    const last = PROPERTY_PALETTE.length - 1;
    const stops = PROPERTY_PALETTE.map((color, index) =>
        `${rgb(color)} ${(100 * index) / last}%`);
    return `linear-gradient(to right, ${stops.join(', ')})`;
}

// Extracts the scientific property result from the versioned viewer envelope.
function characterPropertiesFromPayload(payload)
{
    if (!payload || payload.format !== 'bali-phy-alignment-viewer' || payload.version !== 1)
        throw new Error('Viewer data has an unsupported format or version.');
    if (!payload.character_properties)
        throw new Error('Viewer data does not contain character properties.');
    return payload.character_properties;
}

// Parses the inert embedded JSON object used by the standalone viewer.
function parseViewerPayload(documentObject)
{
    const dataElement = documentObject.getElementById('alignment-viewer-data');
    if (!dataElement)
        return null;
    const payload = JSON.parse(dataElement.textContent);
    return {dataElement, payload, characterProperties: characterPropertiesFromPayload(payload)};
}

// Collects the coordinate metadata and original inline style of every alignment cell.
function collectCells(documentObject)
{
    // Records coordinates once so later recoloring does not repeatedly parse the DOM.
    return Array.from(documentObject.querySelectorAll('td.alignment-cell')).map((element) => {
        const sequence = Number.parseInt(element.dataset.sequence, 10);
        const column = Number.parseInt(element.dataset.column, 10);
        const character = Number.parseInt(element.dataset.character ?? '-1', 10);
        return {
            element,
            sequence,
            column,
            character,
            originalStyle: element.getAttribute('style'),
            value: null,
            count: null,
            uncertainty: null,
        };
    });
}

// Derives displayed sequence names without relying on JSON object insertion order.
function sequenceNamesForViewer(payload, properties, cells)
{
    const sequenceCount = cells.reduce((count, cell) =>
        Math.max(count, Number.isInteger(cell.sequence) ? cell.sequence + 1 : 0), 0);
    const names = Array(sequenceCount).fill(undefined);
    if (Array.isArray(payload.sequences)) {
        // Copies only valid names that correspond to displayed sequence indices.
        payload.sequences.forEach((name, index) => {
            if (index < names.length && typeof name === 'string')
                names[index] = name;
        });
    }
    for (const cell of cells) {
        if (names[cell.sequence] !== undefined)
            continue;
        const row = cell.element.closest('tr');
        const nameCell = row ? row.querySelector('.sequencename') : null;
        if (nameCell)
            names[cell.sequence] = nameCell.textContent.trim();
    }
    const firstProperty = Object.values(properties.properties || {})[0];
    const fallbackNames = firstProperty && firstProperty.mean ? Object.keys(firstProperty.mean) : [];
    return names.map((name, index) => name ?? fallbackNames[index] ?? `Sequence ${index + 1}`);
}

// Normalizes name-keyed property matrices into displayed sequence order.
function collectProperties(characterProperties, sequenceNames)
{
    const propertyMap = characterProperties.properties || {};
    // Reorders every name-keyed matrix to the displayed alignment sequence order.
    return Object.entries(propertyMap).map(([name, property]) => {
        const meanBySequence = sequenceNames.map((sequenceName) =>
            Array.isArray(property.mean && property.mean[sequenceName]) ?
                property.mean[sequenceName] : []);
        const countBySequence = sequenceNames.map((sequenceName) =>
            Array.isArray(property.count && property.count[sequenceName]) ?
                property.count[sequenceName] : []);
        const allMeans = meanBySequence.flat();
        return {
            name,
            meanBySequence,
            countBySequence,
            values: allMeans.filter((value) => Number.isFinite(value)),
            hasMissing: allMeans.some((value) => !Number.isFinite(value)),
        };
    });
}

// Accepts AU fading only for the explicit probability/grid-coordinate contract.
function validatedAlignmentUncertainty(payload, sequenceCount, cells)
{
    const uncertainty = payload.alignment_uncertainty;
    if (!uncertainty || uncertainty.kind !== 'posterior-alignment-probability')
        return null;
    const coordinates = uncertainty.coordinates || {};
    if (coordinates.kind !== 'alignment-grid-cell' || coordinates.index_base !== 0)
        return null;
    if (!Array.isArray(uncertainty.mean) || uncertainty.mean.length !== sequenceCount)
        return null;
    const requiredColumns = cells.reduce((length, cell) =>
        Math.max(length, Number.isInteger(cell.column) ? cell.column + 1 : 0), 0);
    for (const row of uncertainty.mean) {
        if (!Array.isArray(row))
            return null;
        if (row.length < requiredColumns)
            return null;
        if (row.some((value) => value !== null &&
            (!Number.isFinite(value) || value < 0 || value > 1)))
            return null;
    }
    return uncertainty.mean;
}

// Creates an element with optional class and safe text content.
function makeElement(documentObject, tagName, className = '', text = '')
{
    const element = documentObject.createElement(tagName);
    if (className)
        element.className = className;
    if (text !== '')
        element.textContent = text;
    return element;
}

// Appends a select option without interpreting its label as markup.
function addOption(documentObject, select, value, label)
{
    const option = documentObject.createElement('option');
    option.value = value;
    option.textContent = label;
    select.append(option);
    return option;
}

// Wraps a form control in a consistently styled accessible label.
function labelledControl(documentObject, labelText, control)
{
    const label = makeElement(documentObject, 'label', 'alignment-viewer-control');
    label.append(makeElement(documentObject, 'span', 'alignment-viewer-control-label', labelText));
    label.append(control);
    return label;
}

// Formats raw scientific values compactly without replacing them with transformed values.
function formatValue(value)
{
    if (!Number.isFinite(value))
        return 'unavailable';
    const magnitude = Math.abs(value);
    if ((magnitude !== 0 && magnitude < 0.001) || magnitude >= 10000)
        return value.toExponential(4);
    return Number.parseFloat(value.toPrecision(6)).toString();
}

class AlignmentPropertyViewer {
    // Captures viewer data and DOM cells before constructing any controls.
    constructor(documentObject, parsed)
    {
        this.document = documentObject;
        this.payload = parsed.payload;
        this.characterProperties = parsed.characterProperties;
        this.cells = collectCells(documentObject);
        this.sequenceNames = sequenceNamesForViewer(
            this.payload, this.characterProperties, this.cells);
        this.properties = collectProperties(this.characterProperties, this.sequenceNames);
        this.uncertainty = validatedAlignmentUncertainty(
            this.payload, this.sequenceNames.length, this.cells);
        this.displayStates = new Map();
        this.cellByCoordinate = new Map(this.cells.map((cell) =>
            [`${cell.sequence}:${cell.column}`, cell]));
        this.currentProperty = null;
        this.currentScale = null;
        this.currentRange = null;
        this.hoveredCell = null;
        this.pinnedCell = null;
    }

    // Installs controls and event delegation, then displays the first property.
    start()
    {
        if (this.properties.length === 0 || this.cells.length === 0)
            return false;
        this.buildToolbar();
        this.installEvents();
        this.propertySelect.value = 'property:0';
        this.render();
        return true;
    }

    // Builds the complete dependency-free toolbar and legend skeleton.
    buildToolbar()
    {
        const doc = this.document;
        this.toolbar = makeElement(doc, 'section', 'alignment-viewer-toolbar');
        this.toolbar.setAttribute('aria-label', 'Alignment property display');
        this.controlRow = makeElement(doc, 'div', 'alignment-viewer-control-row');

        this.propertySelect = makeElement(doc, 'select', 'alignment-viewer-select');
        this.propertySelect.setAttribute('aria-label', 'Color alignment by');
        addOption(doc, this.propertySelect, 'original', 'Original colors');
        this.properties.forEach((property, index) =>
            addOption(doc, this.propertySelect, `property:${index}`, property.name));
        this.controlRow.append(labelledControl(doc, 'Color by', this.propertySelect));

        this.transformSelect = makeElement(doc, 'select', 'alignment-viewer-select');
        this.transformSelect.setAttribute('aria-label', 'Property scale transform');
        addOption(doc, this.transformSelect, 'linear', 'Linear');
        this.logOption = addOption(doc, this.transformSelect, 'log10', 'Log10');
        addOption(doc, this.transformSelect, 'rank', 'Percentile rank');
        this.controlRow.append(labelledControl(doc, 'Scale', this.transformSelect));

        this.rangeSelect = makeElement(doc, 'select', 'alignment-viewer-select');
        this.rangeSelect.setAttribute('aria-label', 'Property display range');
        addOption(doc, this.rangeSelect, 'robust', 'Robust (2–98%)');
        addOption(doc, this.rangeSelect, 'full', 'Full range');
        addOption(doc, this.rangeSelect, 'custom', 'Custom');
        this.controlRow.append(labelledControl(doc, 'Range', this.rangeSelect));

        this.lowerInput = makeElement(doc, 'input', 'alignment-viewer-number');
        this.lowerInput.type = 'number';
        this.lowerInput.step = 'any';
        this.lowerInput.setAttribute('aria-label', 'Lower property bound');
        this.lowerControl = labelledControl(doc, 'Lower', this.lowerInput);
        this.controlRow.append(this.lowerControl);

        this.upperInput = makeElement(doc, 'input', 'alignment-viewer-number');
        this.upperInput.type = 'number';
        this.upperInput.step = 'any';
        this.upperInput.setAttribute('aria-label', 'Upper property bound');
        this.upperControl = labelledControl(doc, 'Upper', this.upperInput);
        this.controlRow.append(this.upperControl);

        this.auCheckbox = makeElement(doc, 'input', 'alignment-viewer-checkbox');
        this.auCheckbox.type = 'checkbox';
        this.auCheckbox.setAttribute('aria-label', 'Fade property color by alignment uncertainty');
        this.auControl = labelledControl(doc, 'Fade by AU', this.auCheckbox);
        this.auControl.hidden = !this.uncertainty;
        this.controlRow.append(this.auControl);

        this.resetButton = makeElement(doc, 'button', 'alignment-viewer-reset', 'Reset');
        this.resetButton.type = 'button';
        this.controlRow.append(this.resetButton);
        this.toolbar.append(this.controlRow);

        this.status = makeElement(doc, 'div', 'alignment-viewer-status');
        this.status.setAttribute('role', 'status');
        this.status.hidden = true;
        this.toolbar.append(this.status);
        this.buildLegend();
        this.buildTooltip();

        const firstTable = this.document.querySelector('table.sequences');
        if (firstTable)
            firstTable.before(this.toolbar);
        else
            this.document.body.prepend(this.toolbar);
    }

    // Builds reusable legend nodes whose labels and gradients change with the scale.
    buildLegend()
    {
        const doc = this.document;
        this.legend = makeElement(doc, 'figure', 'alignment-viewer-legend');
        this.legend.hidden = true;
        this.legendCaption = makeElement(doc, 'figcaption', 'alignment-viewer-legend-caption');
        this.legend.append(this.legendCaption);
        this.legendPlot = makeElement(doc, 'div', 'alignment-viewer-legend-plot');
        this.legendRamp = makeElement(doc, 'div', 'alignment-viewer-legend-ramp');
        this.legendRamp.setAttribute('role', 'img');
        this.auCertain = makeElement(doc, 'span', 'alignment-viewer-au-certain', 'certain');
        this.auUncertain = makeElement(doc, 'span', 'alignment-viewer-au-uncertain', 'uncertain');
        this.legendPlot.append(this.legendRamp, this.auCertain, this.auUncertain);
        this.legend.append(this.legendPlot);
        this.legendTicks = makeElement(doc, 'div', 'alignment-viewer-legend-ticks');
        this.legend.append(this.legendTicks);
        this.missingKey = makeElement(doc, 'div', 'alignment-viewer-missing-key');
        this.missingKey.append(makeElement(doc, 'span', 'alignment-viewer-missing-swatch'));
        this.missingKey.append(makeElement(doc, 'span', '', 'unavailable'));
        this.legend.append(this.missingKey);
        this.toolbar.append(this.legend);
    }

    // Creates the shared tooltip used by pointer and keyboard navigation.
    buildTooltip()
    {
        this.tooltip = makeElement(this.document, 'div', 'alignment-viewer-tooltip');
        this.tooltip.id = 'alignment-viewer-tooltip';
        this.tooltip.setAttribute('role', 'tooltip');
        this.tooltip.hidden = true;
        this.document.body.append(this.tooltip);
    }

    // Registers one handler per interaction type instead of one per alignment cell.
    installEvents()
    {
        this.propertySelect.addEventListener('change', this.handlePropertyChange.bind(this));
        this.transformSelect.addEventListener('change', this.handleTransformChange.bind(this));
        this.rangeSelect.addEventListener('change', this.handleRangeChange.bind(this));
        this.lowerInput.addEventListener('change', this.handleBoundsChange.bind(this));
        this.upperInput.addEventListener('change', this.handleBoundsChange.bind(this));
        this.auCheckbox.addEventListener('change', this.handleAUChange.bind(this));
        this.resetButton.addEventListener('click', this.handleReset.bind(this));
        this.document.addEventListener('pointerover', this.handlePointerOver.bind(this));
        this.document.addEventListener('pointermove', this.handlePointerMove.bind(this));
        this.document.addEventListener('pointerout', this.handlePointerOut.bind(this));
        this.document.addEventListener('click', this.handleCellClick.bind(this));
        this.document.addEventListener('focusin', this.handleFocusIn.bind(this));
        this.document.addEventListener('focusout', this.handleFocusOut.bind(this));
        this.document.addEventListener('keydown', this.handleKeydown.bind(this));
    }

    // Returns the selected property definition, or null for the original view.
    selectedProperty()
    {
        if (!this.propertySelect.value.startsWith('property:'))
            return null;
        const index = Number.parseInt(this.propertySelect.value.slice('property:'.length), 10);
        return this.properties[index] || null;
    }

    // Returns stable per-property display settings, creating defaults on first use.
    stateFor(property)
    {
        if (!this.displayStates.has(property.name)) {
            this.displayStates.set(property.name, {
                transform: preferredTransform(property.name, property.values),
                range: 'robust',
                customLower: null,
                customUpper: null,
                fadeByAU: false,
            });
        }
        return this.displayStates.get(property.name);
    }

    // Resolves the active automatic or user-specified property range.
    boundsFor(property, state)
    {
        if (state.transform === 'rank')
            return {};
        if (state.range !== 'custom')
            return automaticScaleBounds(property.values, state.transform, state.range);
        if (!Number.isFinite(state.customLower) || !Number.isFinite(state.customUpper))
            throw new RangeError('Enter finite lower and upper custom bounds.');
        return {lower: state.customLower, upper: state.customUpper};
    }

    // Synchronizes enabled controls with the currently selected property state.
    updateControls(property, state, bounds)
    {
        const original = !property;
        const unavailable = !original && property.values.length === 0;
        this.transformSelect.disabled = original || unavailable;
        this.rangeSelect.disabled = original || unavailable || state.transform === 'rank';
        this.logOption.disabled = unavailable ||
            (!original && !canUseLogScale(property.name, property.values));
        if (!original && state.transform === 'log10' && this.logOption.disabled)
            state.transform = 'linear';
        if (!original) {
            this.transformSelect.value = state.transform;
            this.rangeSelect.value = state.range;
        }
        const custom = !original && !unavailable &&
            state.transform !== 'rank' && state.range === 'custom';
        this.lowerInput.disabled = !custom;
        this.upperInput.disabled = !custom;
        this.lowerControl.classList.toggle('alignment-viewer-control-disabled', !custom);
        this.upperControl.classList.toggle('alignment-viewer-control-disabled', !custom);
        if (!original && !unavailable && state.transform !== 'rank') {
            this.lowerInput.value = String(bounds.lower);
            this.upperInput.value = String(bounds.upper);
        }
        else if (unavailable) {
            this.lowerInput.value = '';
            this.upperInput.value = '';
        }
        this.auCheckbox.disabled = original || unavailable || !this.uncertainty;
        this.auCheckbox.checked = !original && !unavailable &&
            state.fadeByAU && Boolean(this.uncertainty);
    }

    // Restores the exact static C++ style and clears dynamic annotation classes.
    restoreCell(cell)
    {
        if (cell.originalStyle === null)
            cell.element.removeAttribute('style');
        else
            cell.element.setAttribute('style', cell.originalStyle);
        cell.element.classList.remove(
            'alignment-property-colored',
            'alignment-property-missing',
            'alignment-au-missing');
        cell.value = null;
        cell.count = null;
        cell.uncertainty = null;
    }

    // Looks up a property mean using displayed sequence and ungapped-character indices.
    propertyValue(property, cell)
    {
        if (cell.character < 0)
            return null;
        const values = property.meanBySequence[cell.sequence];
        return values ? values[cell.character] : null;
    }

    // Looks up the retained sample count corresponding to a displayed character.
    propertyCount(property, cell)
    {
        if (cell.character < 0)
            return null;
        const counts = property.countBySequence[cell.sequence];
        return counts ? counts[cell.character] : null;
    }

    // Looks up a grid-cell AU probability from the explicit sequence-major matrix.
    uncertaintyValue(cell)
    {
        if (!this.uncertainty || !this.uncertainty[cell.sequence])
            return null;
        const value = this.uncertainty[cell.sequence][cell.column];
        return Number.isFinite(value) ? value : null;
    }

    // Applies the selected property scale and optional AU fade to every visible cell.
    colorCells(property, scale, fadeByAU)
    {
        for (const cell of this.cells) {
            this.restoreCell(cell);
            if (cell.character < 0)
                continue;
            const value = this.propertyValue(property, cell);
            cell.value = Number.isFinite(value) ? value : null;
            cell.count = this.propertyCount(property, cell);
            if (fadeByAU)
                cell.uncertainty = this.uncertaintyValue(cell);
            if (!Number.isFinite(value)) {
                cell.element.classList.add('alignment-property-missing');
                continue;
            }
            let color = paletteColor(scale.normalize(value));
            if (fadeByAU) {
                if (!Number.isFinite(cell.uncertainty)) {
                    cell.element.classList.add('alignment-au-missing');
                    continue;
                }
                color = blendWithWhite(color, cell.uncertainty);
            }
            const foreground = contrastingTextColor(color);
            cell.element.style.background = rgb(color);
            cell.element.style.color = rgb(foreground);
            cell.element.classList.add('alignment-property-colored');
        }
    }

    // Makes one non-gap cell keyboard reachable while retaining a compact tab order.
    updateRovingTabIndex()
    {
        const navigable = this.cells.filter((cell) => cell.character >= 0);
        const current = navigable.find((cell) => cell.element.tabIndex === 0);
        for (const cell of navigable)
            cell.element.tabIndex = -1;
        const target = current || navigable[0];
        if (target)
            target.element.tabIndex = 0;
    }

    // Rebuilds the numeric legend, including clipping marks and optional AU dimension.
    updateLegend(property, state, scale)
    {
        const transformLabels = {
            linear: 'linear',
            log10: 'log10',
            rank: 'percentile rank',
        };
        const rangeLabel = state.transform === 'rank' ? '' :
            state.range === 'robust' ? ' · robust 2–98%' :
            state.range === 'full' ? ' · full range' : ' · custom range';
        this.legendCaption.textContent = `${property.name} · ${transformLabels[state.transform]}${rangeLabel}`;
        const fadeByAU = state.fadeByAU && Boolean(this.uncertainty);
        this.legend.classList.toggle('alignment-viewer-legend-2d', fadeByAU);
        this.legendRamp.style.backgroundImage = fadeByAU ?
            `linear-gradient(to top, rgba(255, 255, 255, 1), rgba(255, 255, 255, 0)), ${paletteGradient()}` :
            paletteGradient();
        this.legendRamp.setAttribute('aria-label', fadeByAU ?
            `${property.name} color by horizontal position and alignment certainty vertically` :
            `${property.name} color scale`);
        this.auCertain.hidden = !fadeByAU;
        this.auUncertain.hidden = !fadeByAU;

        const full = state.transform === 'rank' ?
            {lower: scale.lower, upper: scale.upper} :
            automaticScaleBounds(property.values, state.transform, 'full');
        const clippedLower = state.transform !== 'rank' && scale.lower > full.lower;
        const clippedUpper = state.transform !== 'rank' && scale.upper < full.upper;
        const clippedZero = state.transform === 'log10' &&
            property.values.some((value) => value === 0);
        this.legendTicks.replaceChildren();
        const ticks = scale.legendTicks(5);
        // Positions raw-value labels under their corresponding palette coordinates.
        ticks.forEach((tick, index) => {
            const label = makeElement(this.document, 'span', 'alignment-viewer-legend-tick');
            label.style.left = `${100 * tick.position}%`;
            let text = formatValue(tick.value);
            if (index === 0 && (clippedLower || clippedZero))
                text = `≤ ${text}`;
            if (index === 0 && clippedZero)
                text += ' (includes 0)';
            if (index === ticks.length - 1 && clippedUpper)
                text = `≥ ${text}`;
            label.textContent = text;
            this.legendTicks.append(label);
        });
        const missingAU = fadeByAU && this.uncertainty.some((row) => row.some((value) => value === null));
        this.missingKey.hidden = !property.hasMissing && !missingAU;
        this.legend.hidden = false;
    }

    // Displays a non-blocking configuration error without discarding prior colors.
    showStatus(message)
    {
        this.status.textContent = message;
        this.status.hidden = false;
        this.toolbar.classList.add('alignment-viewer-has-error');
    }

    // Clears the configuration status after a successful render.
    clearStatus()
    {
        this.status.textContent = '';
        this.status.hidden = true;
        this.toolbar.classList.remove('alignment-viewer-has-error');
    }

    // Restores the static document when Original colors is selected.
    renderOriginal()
    {
        for (const cell of this.cells) {
            this.restoreCell(cell);
            cell.element.removeAttribute('tabindex');
            cell.element.removeAttribute('aria-describedby');
        }
        this.currentProperty = null;
        this.currentScale = null;
        this.currentRange = null;
        this.legend.hidden = true;
        this.hideTooltip(true);
        this.updateControls(null, {transform: 'linear', range: 'robust'}, {});
        this.clearStatus();
    }

    // Applies all current controls atomically, leaving prior colors on invalid input.
    render()
    {
        const property = this.selectedProperty();
        if (!property) {
            this.renderOriginal();
            return;
        }
        if (property.values.length === 0) {
            const state = this.stateFor(property);
            this.updateControls(property, state, {});
            for (const cell of this.cells) {
                this.restoreCell(cell);
                if (cell.character < 0)
                    continue;
                cell.count = this.propertyCount(property, cell);
                cell.uncertainty = state.fadeByAU ? this.uncertaintyValue(cell) : null;
                cell.element.classList.add('alignment-property-missing');
            }
            this.currentProperty = property;
            this.currentScale = null;
            this.currentRange = state.range;
            this.legend.hidden = true;
            this.updateRovingTabIndex();
            this.showStatus(`${property.name} has no finite posterior means.`);
            return;
        }
        const state = this.stateFor(property);
        if (state.transform === 'log10' && !canUseLogScale(property.name, property.values))
            state.transform = 'linear';
        try {
            const bounds = this.boundsFor(property, state);
            const scale = createScale(property.values, {transform: state.transform, ...bounds});
            this.updateControls(property, state, bounds);
            this.colorCells(property, scale, state.fadeByAU && Boolean(this.uncertainty));
            this.currentProperty = property;
            this.currentScale = scale;
            this.currentRange = state.range;
            this.updateLegend(property, state, scale);
            this.updateRovingTabIndex();
            this.clearStatus();
            if (this.pinnedCell)
                this.showTooltip(this.pinnedCell);
        }
        catch (error) {
            this.showStatus(error.message);
        }
    }

    // Switches property while retaining independent display settings for each one.
    handlePropertyChange()
    {
        this.hideTooltip(true);
        this.render();
    }

    // Changes the numerical transform for the active property.
    handleTransformChange()
    {
        const property = this.selectedProperty();
        if (!property)
            return;
        const state = this.stateFor(property);
        state.transform = this.transformSelect.value;
        if (state.transform === 'log10' && state.range === 'custom' &&
            (!(state.customLower > 0) || !(state.customUpper > 0))) {
            const robust = automaticScaleBounds(property.values, 'log10', 'robust');
            state.customLower = robust.lower;
            state.customUpper = robust.upper;
        }
        this.render();
    }

    // Changes automatic range policy and seeds custom bounds from the robust range.
    handleRangeChange()
    {
        const property = this.selectedProperty();
        if (!property)
            return;
        const state = this.stateFor(property);
        state.range = this.rangeSelect.value;
        if (state.range === 'custom' &&
            (!Number.isFinite(state.customLower) || !Number.isFinite(state.customUpper))) {
            const robust = automaticScaleBounds(property.values, state.transform, 'robust');
            state.customLower = robust.lower;
            state.customUpper = robust.upper;
        }
        this.render();
    }

    // Stores custom raw-unit bounds after either number input changes.
    handleBoundsChange()
    {
        const property = this.selectedProperty();
        if (!property)
            return;
        const state = this.stateFor(property);
        state.customLower = this.lowerInput.valueAsNumber;
        state.customUpper = this.upperInput.valueAsNumber;
        this.render();
    }

    // Enables or disables the explicit alignment-certainty fade layer.
    handleAUChange()
    {
        const property = this.selectedProperty();
        if (!property)
            return;
        this.stateFor(property).fadeByAU = this.auCheckbox.checked;
        this.render();
    }

    // Restores the selected property's documented default display settings.
    handleReset()
    {
        const property = this.selectedProperty();
        if (property)
            this.displayStates.delete(property.name);
        this.render();
    }

    // Returns a managed cell for an event target, ignoring unrelated table cells.
    cellForTarget(target)
    {
        if (!target || typeof target.closest !== 'function')
            return null;
        const element = target.closest('td.alignment-cell');
        if (!element)
            return null;
        return this.cells.find((cell) => cell.element === element) || null;
    }

    // Adds one safe term/value pair to the tooltip definition list.
    appendTooltipRow(list, term, value)
    {
        list.append(makeElement(this.document, 'dt', '', term));
        list.append(makeElement(this.document, 'dd', '', value));
    }

    // Populates the tooltip with raw coordinates, posterior mean, count, and AU.
    fillTooltip(cell)
    {
        const list = makeElement(this.document, 'dl', 'alignment-viewer-tooltip-data');
        this.appendTooltipRow(list, 'Sequence', this.sequenceNames[cell.sequence]);
        this.appendTooltipRow(list, 'Alignment column', String(cell.column + 1));
        this.appendTooltipRow(list, 'Sequence character', String(cell.character + 1));
        this.appendTooltipRow(list, 'Character', cell.element.textContent.trim());
        this.appendTooltipRow(list, this.currentProperty.name, formatValue(cell.value));
        if (Number.isFinite(cell.count))
            this.appendTooltipRow(list, 'Samples', String(cell.count));
        const state = this.stateFor(this.currentProperty);
        if (state.fadeByAU && this.uncertainty)
            this.appendTooltipRow(list, 'Alignment certainty', formatValue(cell.uncertainty));
        const transformLabel = state.transform === 'rank' ? 'percentile rank' : state.transform;
        const rangeLabel = state.transform === 'rank' ? '' : `, ${state.range}`;
        this.appendTooltipRow(list, 'Display scale', `${transformLabel}${rangeLabel}`);
        this.tooltip.replaceChildren(list);
    }

    // Places the tooltip within the viewport near pointer or focus coordinates.
    positionTooltip(clientX, clientY)
    {
        const margin = 10;
        const desiredLeft = clientX + 14;
        const desiredTop = clientY + 14;
        const left = Math.min(desiredLeft,
            globalScope.innerWidth - this.tooltip.offsetWidth - margin);
        const top = Math.min(desiredTop,
            globalScope.innerHeight - this.tooltip.offsetHeight - margin);
        this.tooltip.style.left = `${Math.max(margin, left)}px`;
        this.tooltip.style.top = `${Math.max(margin, top)}px`;
    }

    // Shows the shared tooltip for a non-gap cell in the active property view.
    showTooltip(cell, clientX = null, clientY = null)
    {
        if (!this.currentProperty || !cell || cell.character < 0)
            return;
        if (this.hoveredCell && this.hoveredCell !== cell)
            this.hoveredCell.element.removeAttribute('aria-describedby');
        this.hoveredCell = cell;
        this.fillTooltip(cell);
        this.tooltip.hidden = false;
        cell.element.setAttribute('aria-describedby', this.tooltip.id);
        if (Number.isFinite(clientX) && Number.isFinite(clientY))
            this.positionTooltip(clientX, clientY);
        else {
            const rectangle = cell.element.getBoundingClientRect();
            this.positionTooltip(rectangle.right, rectangle.top);
        }
    }

    // Hides the tooltip, optionally also clearing a click-pinned cell.
    hideTooltip(clearPin = false)
    {
        if (this.hoveredCell)
            this.hoveredCell.element.removeAttribute('aria-describedby');
        this.hoveredCell = null;
        if (clearPin)
            this.pinnedCell = null;
        this.tooltip.hidden = true;
    }

    // Shows tooltip content when a pointer enters a managed alignment cell.
    handlePointerOver(event)
    {
        const cell = this.cellForTarget(event.target);
        if (cell && !this.pinnedCell)
            this.showTooltip(cell, event.clientX, event.clientY);
    }

    // Keeps an unpinned pointer tooltip close to the current cursor position.
    handlePointerMove(event)
    {
        if (this.hoveredCell && !this.pinnedCell)
            this.positionTooltip(event.clientX, event.clientY);
    }

    // Hides a pointer tooltip after leaving its cell unless the tooltip is pinned.
    handlePointerOut(event)
    {
        const cell = this.cellForTarget(event.target);
        if (!cell || this.pinnedCell)
            return;
        if (event.relatedTarget && cell.element.contains(event.relatedTarget))
            return;
        this.hideTooltip();
    }

    // Pins a tooltip for mouse/touch inspection and moves keyboard focus to its cell.
    handleCellClick(event)
    {
        const cell = this.cellForTarget(event.target);
        if (!cell || !this.currentProperty || cell.character < 0)
            return;
        if (this.pinnedCell === cell) {
            this.hideTooltip(true);
            return;
        }
        this.pinnedCell = cell;
        for (const candidate of this.cells) {
            if (candidate.character >= 0)
                candidate.element.tabIndex = candidate === cell ? 0 : -1;
            else
                candidate.element.removeAttribute('tabindex');
        }
        cell.element.focus({preventScroll: true});
        this.showTooltip(cell, event.clientX, event.clientY);
    }

    // Opens a tooltip when keyboard focus reaches a managed cell.
    handleFocusIn(event)
    {
        const cell = this.cellForTarget(event.target);
        if (cell) {
            if (this.pinnedCell)
                this.pinnedCell = cell;
            this.showTooltip(cell);
        }
    }

    // Closes an unpinned tooltip after keyboard focus leaves a cell.
    handleFocusOut(event)
    {
        const cell = this.cellForTarget(event.target);
        if (cell && !this.pinnedCell)
            this.hideTooltip();
    }

    // Finds the next non-gap grid cell in a requested keyboard direction.
    navigableNeighbor(cell, sequenceStep, columnStep)
    {
        let sequence = cell.sequence + sequenceStep;
        let column = cell.column + columnStep;
        while (sequence >= 0 && column >= 0) {
            const candidate = this.cellByCoordinate.get(`${sequence}:${column}`);
            if (!candidate)
                return null;
            if (candidate.character >= 0)
                return candidate;
            sequence += sequenceStep;
            column += columnStep;
        }
        return null;
    }

    // Provides roving arrow-key navigation and Escape-to-close behavior.
    handleKeydown(event)
    {
        if (event.key === 'Escape') {
            this.hideTooltip(true);
            return;
        }
        const cell = this.cellForTarget(event.target);
        if (!cell)
            return;
        const directions = {
            ArrowLeft: [0, -1],
            ArrowRight: [0, 1],
            ArrowUp: [-1, 0],
            ArrowDown: [1, 0],
        };
        const direction = directions[event.key];
        if (!direction)
            return;
        const neighbor = this.navigableNeighbor(cell, direction[0], direction[1]);
        if (!neighbor)
            return;
        event.preventDefault();
        cell.element.tabIndex = -1;
        neighbor.element.tabIndex = 0;
        neighbor.element.focus();
    }
}

// Initializes one standalone alignment property viewer from embedded data and cell hooks.
function initializeAlignmentViewer(documentObject = globalScope.document)
{
    try {
        const parsed = parseViewerPayload(documentObject);
        if (!parsed)
            return null;
        const viewer = new AlignmentPropertyViewer(documentObject, parsed);
        viewer.start();
        return viewer;
    }
    catch (error) {
        const message = makeElement(documentObject, 'p', 'alignment-viewer-load-error',
            `Character-property viewer unavailable: ${error.message}`);
        const firstTable = documentObject.querySelector('table.sequences');
        if (firstTable)
            firstTable.before(message);
        else
            documentObject.body.prepend(message);
        if (globalScope.console)
            globalScope.console.error(error);
        return null;
    }
}

const api = {
    createScale,
    automaticScaleBounds,
    canUseLogScale,
    preferredTransform,
    blendWithWhite,
    initializeAlignmentViewer,
};

if (typeof module !== 'undefined' && module.exports)
    module.exports = api;

if (globalScope && globalScope.document) {
    globalScope.BaliPhyAlignmentViewer = api;
    if (globalScope.document.readyState === 'loading')
        globalScope.document.addEventListener('DOMContentLoaded',
            () => initializeAlignmentViewer(globalScope.document), {once: true});
    else
        initializeAlignmentViewer(globalScope.document);
}

})(typeof globalThis === 'undefined' ? this : globalThis);
