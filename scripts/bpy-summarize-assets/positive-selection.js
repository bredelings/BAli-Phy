// Each table owns its sort state; the original probability rank controls top-15 membership.
// Build controls only when scripting works, leaving a usable static report otherwise.
document.querySelectorAll('.positive-selection-table').forEach(section => {
    const table = section.querySelector('table');
    const head = table.tHead;
    const body = table.tBodies[0];
    let sort = {column: 3, direction: -1};
    // Preserve the heading text as the button label, including under grouped headings.
    head.querySelectorAll('th[data-sort-column]').forEach(cell => {
        const button = document.createElement('button');
        button.type = 'button';
        button.dataset.sortColumn = cell.dataset.sortColumn;
        button.dataset.defaultDirection = cell.dataset.defaultDirection;
        button.dataset.sortType = 'number';
        button.dataset.label = cell.textContent;
        button.textContent = cell.textContent;
        cell.replaceChildren(button);
    });
    // A new heading starts in its natural direction; another click reverses that direction.
    head.addEventListener('click', event => {
        const button = event.target.closest('button[data-sort-column]');
        if (!button || !head.contains(button))
            return;
        const column = Number(button.dataset.sortColumn);
        sort = {column, direction: column === sort.column ?
            -sort.direction : Number(button.dataset.defaultDirection)};
        sortTableRows(head, body, sort);
    });
    sortTableRows(head, body, sort);
    const expand = section.querySelector('.site-expand');
    if (expand) {
        const checkbox = expand.querySelector('input');
        checkbox.checked = false;
        // Sorting never changes the selected subset: visibility follows the original rank.
        checkbox.addEventListener('change', () => {
            const rows = Array.from(body.rows);
            rows.forEach(row => { row.hidden = !checkbox.checked && Number(row.dataset.reportRow) >= 15; });
            section.querySelector('.site-count').textContent = checkbox.checked ?
                `Showing all ${rows.length} selected columns.` : `Showing 15 of ${rows.length} selected columns.`;
        });
        expand.hidden = false;
    }
});
