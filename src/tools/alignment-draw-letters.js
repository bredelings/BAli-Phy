// Keeps letter visibility independent of color controls, including on AU-only pages.
(function () {
    'use strict';

    // Hide glyphs through CSS so cell geometry, hover targets, and text remain available.
    function initializeLetters() {
        if (!document.querySelector('.alignment-residue'))
            return;
        const row = document.createElement('div');
        row.className = 'alignment-letter-controls';
        const label = document.createElement('label');
        const checkbox = document.createElement('input');
        checkbox.type = 'checkbox';
        checkbox.checked = true;
        label.append(checkbox, ' Show letters');
        row.append(label);
        const toolbar = document.querySelector('.alignment-viewer-toolbar');
        if (toolbar)
            toolbar.querySelector('.alignment-viewer-control-row').after(row);
        else
            document.querySelector('table.sequences').before(row);
        checkbox.addEventListener('change', () => {
            document.body.classList.toggle('alignment-hide-letters', !checkbox.checked);
        });
    }

    if (document.readyState === 'loading')
        document.addEventListener('DOMContentLoaded', initializeLetters, {once: true});
    else
        initializeLetters();
})();
