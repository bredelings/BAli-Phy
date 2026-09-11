// Sort existing rows by unrounded cell values; ties retain their current order.
// Explicit cell indices allow grouped headings and headings without sorting controls.
function sortTableRows(head, body, sort)
{
    const buttons = Array.from(head.querySelectorAll('button[data-sort-column]'));
    const active = buttons.find(button => Number(button.dataset.sortColumn) === sort.column);
    if (!active)
        return;
    const type = active.dataset.sortType;
    const rows = Array.from(body.querySelectorAll('tr[data-report-row]'));
    // Compare unformatted values so display rounding cannot change their ordering.
    // Treat unavailable numeric values as equal instead of inventing a secondary key.
    rows.sort((first, second) => {
        const firstValue = first.cells[sort.column].dataset.sortValue;
        const secondValue = second.cells[sort.column].dataset.sortValue;
        let difference = type === 'number' ? Number(firstValue) - Number(secondValue) :
            firstValue.localeCompare(secondValue);
        if (!Number.isFinite(difference))
            difference = 0;
        return sort.direction * difference;
    });
    body.append(...rows);
    // Mark only the active header with its direction and restore every button's base label.
    // Rewriting all headers also clears the indicator left by the previously sorted column.
    buttons.forEach(button => {
        const selected = Number(button.dataset.sortColumn) === sort.column;
        button.parentElement.setAttribute('aria-sort', selected ?
            (sort.direction > 0 ? 'ascending' : 'descending') : 'none');
        button.textContent = button.dataset.label + (selected ?
            (sort.direction > 0 ? ' ▲' : ' ▼') : '');
    });
}
