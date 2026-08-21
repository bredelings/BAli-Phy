// Decodes alignment-viewer payloads independently of DOM presentation.
(function (globalScope) {
'use strict';

// Extracts the scientific property result from the versioned viewer envelope.
function characterPropertiesFromPayload(payload)
{
    if (!payload || payload.format !== 'bali-phy-alignment-viewer' || payload.version !== 2)
        throw new Error('Viewer data has an unsupported format or version.');
    if (!payload.character_properties)
        throw new Error('Viewer data does not contain character properties.');
    return payload.character_properties;
}

// Reads one object-valued C++ report field.
function reportsFromPayload(payload, field = 'character_property_reports')
{
    const reports = payload[field];
    return reports && typeof reports === 'object' && !Array.isArray(reports) ? reports : {};
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
            aminoAcid: element.dataset.aminoAcid || null,
            originalStyle: element.getAttribute('style'),
            statistics: null,
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
        const sdBySequence = sequenceNames.map((sequenceName) =>
            Array.isArray(property.sd && property.sd[sequenceName]) ? property.sd[sequenceName] : []);
        const medianBySequence = sequenceNames.map((sequenceName) =>
            Array.isArray(property.median && property.median[sequenceName]) ? property.median[sequenceName] : []);
        const allMeans = meanBySequence.flat();
        return {
            name,
            meanBySequence,
            sdBySequence,
            medianBySequence,
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

const api = {
    parseViewerPayload,
    reportsFromPayload,
    collectCells,
    sequenceNamesForViewer,
    collectProperties,
    validatedAlignmentUncertainty,
};

if (typeof module !== 'undefined' && module.exports)
    module.exports = api;

if (globalScope)
    globalScope.BaliPhyAlignmentProperties = api;

})(typeof globalThis === 'undefined' ? this : globalThis);
