// Implements alignment-viewer controls and DOM interaction using the shared data and scale modules.
(function (globalScope) {
'use strict';

const {
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
} = globalScope.BaliPhyAlignmentScales;

const {
    parseViewerPayload,
    collectCells,
    sequenceNamesForViewer,
    collectProperties,
    validatedAlignmentUncertainty,
} = globalScope.BaliPhyAlignmentProperties;

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

        this.paletteSelect = makeElement(doc, 'select', 'alignment-viewer-select');
        this.paletteSelect.setAttribute('aria-label', 'Property color palette');
        for (const [name, palette] of Object.entries(PROPERTY_PALETTES))
            addOption(doc, this.paletteSelect, name, palette.label);
        this.controlRow.append(labelledControl(doc, 'Palette', this.paletteSelect));

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
        this.paletteSelect.addEventListener('change', this.handlePaletteChange.bind(this));
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
                palette: 'viridis',
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
        this.paletteSelect.disabled = original || unavailable;
        this.rangeSelect.disabled = original || unavailable || state.transform === 'rank';
        this.logOption.disabled = unavailable ||
            (!original && !canUseLogScale(property.name, property.values));
        if (!original && state.transform === 'log10' && this.logOption.disabled)
            state.transform = 'linear';
        if (!original) {
            this.transformSelect.value = state.transform;
            this.paletteSelect.value = state.palette;
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
    colorCells(property, scale, paletteName, fadeByAU)
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
            let color = paletteColor(scale.normalize(value), paletteName);
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
        const palette = propertyPalette(state.palette);
        const centerLabel = scale.diverging ? ` · median ${formatValue(scale.center)}` : '';
        this.legendCaption.textContent =
            `${property.name} · ${transformLabels[state.transform]}${rangeLabel} · ${palette.label}${centerLabel}`;
        const fadeByAU = state.fadeByAU && Boolean(this.uncertainty);
        this.legend.classList.toggle('alignment-viewer-legend-2d', fadeByAU);
        this.legendRamp.style.backgroundImage = fadeByAU ?
            `linear-gradient(to top, rgba(255, 255, 255, 1), rgba(255, 255, 255, 0)), ${paletteGradient(state.palette)}` :
            paletteGradient(state.palette);
        this.legendRamp.setAttribute('aria-label', fadeByAU ?
            `${property.name} ${palette.label} color by horizontal position and alignment certainty vertically` :
            `${property.name} ${palette.label} color scale${scale.diverging ? ' centered on the median' : ''}`);
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
        ticks.forEach((tick) => {
            const label = makeElement(this.document, 'span', 'alignment-viewer-legend-tick');
            label.style.left = `${100 * tick.position}%`;
            if (tick.position === 0)
                label.classList.add('alignment-viewer-legend-tick-lower');
            if (tick.position === 1)
                label.classList.add('alignment-viewer-legend-tick-upper');
            let text = formatValue(tick.value);
            if (tick.position === 0 && (clippedLower || clippedZero))
                text = `≤ ${text}`;
            if (tick.position === 0 && clippedZero)
                text += ' (includes 0)';
            if (tick.position === 1 && clippedUpper)
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
        this.updateControls(
            null, {transform: 'linear', palette: 'viridis', range: 'robust'}, {});
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
            const baseScale = createScale(
                property.values, {transform: state.transform, ...bounds});
            const palette = propertyPalette(state.palette);
            const sorted = [...property.values].sort((left, right) => left - right);
            const scale = palette.kind === 'diverging' ?
                createDivergingScale(baseScale, quantile(sorted, 0.5)) : baseScale;
            this.updateControls(property, state, bounds);
            this.colorCells(
                property, scale, state.palette,
                state.fadeByAU && Boolean(this.uncertainty));
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

    // Changes the color palette while retaining the property's numerical transform.
    handlePaletteChange()
    {
        const property = this.selectedProperty();
        if (!property)
            return;
        this.stateFor(property).palette = this.paletteSelect.value;
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
        this.appendTooltipRow(list, 'Palette', propertyPalette(state.palette).label);
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
    initializeAlignmentViewer,
};

if (globalScope && globalScope.document) {
    globalScope.BaliPhyAlignmentViewer = api;
    if (globalScope.document.readyState === 'loading')
        globalScope.document.addEventListener('DOMContentLoaded',
            () => initializeAlignmentViewer(globalScope.document), {once: true});
    else
        initializeAlignmentViewer(globalScope.document);
}

})(typeof globalThis === 'undefined' ? this : globalThis);
