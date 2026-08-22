// Defines dependency-free numerical scales and color palettes for alignment viewers.
(function (globalScope) {
'use strict';

const PROPERTY_PALETTES = {
    viridis: {
        label: 'Viridis',
        kind: 'sequential',
        // Five representative stops from the perceptually uniform, CC0 palette.
        stops: [[68, 1, 84], [59, 82, 139], [33, 145, 140], [94, 201, 98], [253, 231, 37]],
    },
    'blue-red': {
        label: 'Blue–red',
        kind: 'sequential',
        stops: [[8, 48, 107], [33, 113, 181], [123, 50, 148], [194, 42, 91], [239, 59, 44]],
    },
    'blue-gray-red': {
        label: 'Blue–gray–red',
        kind: 'diverging',
        stops: [[33, 102, 172], [103, 169, 207], [232, 232, 232], [239, 138, 98], [178, 24, 43]],
    },
};

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

    // Converts a palette position back into a raw value in transformed scale space.
    function valueAt(position)
    {
        if (lower === upper)
            return lower;
        const transformed = transformedLower +
            clamp(position, 0, 1) * (transformedUpper - transformedLower);
        return inverse(transformed);
    }

    // Returns raw values and normalized positions for a numeric legend.
    function legendTicks(count)
    {
        return continuousLegendTicks(count, lower, upper, transform, inverse);
    }

    return {normalize, valueAt, legendTicks, lower, upper, transform: options.transform};
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
            valueAt: () => constant,
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
        valueAt: (position) => valueAtRank(groups, clamp(position, 0, 1)),
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
    if (propertyName === 'posSelection' || propertyName.endsWith('-posSelection') || values.length === 0)
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

// Re-centers an existing scale so the supplied raw median occupies the palette midpoint.
function createDivergingScale(baseScale, center)
{
    if (!baseScale || typeof baseScale.normalize !== 'function' ||
        typeof baseScale.valueAt !== 'function')
        throw new TypeError('A diverging scale needs an invertible base scale.');
    if (!Number.isFinite(center))
        throw new RangeError('A diverging scale center must be finite.');
    const centerPosition = baseScale.normalize(center);
    if (!Number.isFinite(centerPosition))
        throw new RangeError('A diverging scale center must lie in the scale domain.');
    if (baseScale.lower === baseScale.upper) {
        return {
            ...baseScale,
            center,
            diverging: true,
        };
    }

    // Maps each side of the center independently onto half of the color palette.
    function normalize(value)
    {
        if (!Number.isFinite(value))
            return null;
        if (value === center)
            return 0.5;
        const position = baseScale.normalize(value);
        if (value < center)
            return centerPosition === 0 ? 0 : 0.5 * position / centerPosition;
        return centerPosition === 1 ? 1 :
            0.5 + 0.5 * (position - centerPosition) / (1 - centerPosition);
    }

    // Converts a diverging palette coordinate back through the corresponding half.
    function valueAt(position)
    {
        const clipped = clamp(position, 0, 1);
        if (clipped === 0.5)
            return center;
        if (clipped < 0.5)
            return baseScale.valueAt(2 * clipped * centerPosition);
        return baseScale.valueAt(
            centerPosition + 2 * (clipped - 0.5) * (1 - centerPosition));
    }

    // Places legend ticks evenly in the two independently normalized palette halves.
    function legendTicks(count)
    {
        if (!Number.isInteger(count) || count < 2)
            throw new RangeError('A legend needs at least two ticks.');
        const firstPosition = centerPosition === 0 ? 0.5 : 0;
        const lastPosition = centerPosition === 1 ? 0.5 : 1;
        return Array.from({length: count}, (_, index) => {
            const fraction = index / (count - 1);
            const position = firstPosition + fraction * (lastPosition - firstPosition);
            return {position, value: valueAt(position)};
        });
    }

    return {
        normalize,
        valueAt,
        legendTicks,
        lower: baseScale.lower,
        upper: baseScale.upper,
        transform: baseScale.transform,
        center,
        diverging: true,
    };
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

// Returns a named palette definition or rejects stale and malformed UI state.
function propertyPalette(name)
{
    const palette = PROPERTY_PALETTES[name];
    if (!palette)
        throw new RangeError(`Unknown property palette: ${name}`);
    return palette;
}

// Interpolates a named property palette at a normalized coordinate.
function paletteColor(position, paletteName = 'viridis')
{
    const stops = propertyPalette(paletteName).stops;
    const scaled = clamp(position, 0, 1) * (stops.length - 1);
    const lower = Math.floor(scaled);
    const upper = Math.ceil(scaled);
    const fraction = scaled - lower;
    return stops[lower].map((channel, index) =>
        Math.round(channel * (1 - fraction) + stops[upper][index] * fraction));
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

// Produces the CSS gradient shared by one- and two-dimensional named-palette legends.
function paletteGradient(paletteName = 'viridis')
{
    const colors = propertyPalette(paletteName).stops;
    const last = colors.length - 1;
    const stops = colors.map((color, index) =>
        `${rgb(color)} ${(100 * index) / last}%`);
    return `linear-gradient(to right, ${stops.join(', ')})`;
}

const api = {
    PROPERTY_PALETTES,
    quantile,
    createScale,
    createDivergingScale,
    automaticScaleBounds,
    canUseLogScale,
    preferredTransform,
    propertyPalette,
    paletteColor,
    paletteGradient,
    blendWithWhite,
    contrastingTextColor,
    rgb,
};

if (typeof module !== 'undefined' && module.exports)
    module.exports = api;

if (globalScope)
    globalScope.BaliPhyAlignmentScales = api;

})(typeof globalThis === 'undefined' ? this : globalThis);
