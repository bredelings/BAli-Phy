// Accessibility adapter for xslTNG's panel; upstream owns opening, closing, search, and links.
// Remove this adapter when upstream provides keyboard controls and manages hidden-panel focus.
// Wait for DOMContentLoaded because the upstream deferred script follows this one.
document.addEventListener('DOMContentLoaded', () => {
    const panel = document.querySelector('nav.toc');
    const opener = document.querySelector('nav.tocopen');
    if (!panel || !opener) return;

    panel.id = 'guide-contents';
    panel.setAttribute('aria-label', 'Table of contents');
    opener.setAttribute('aria-controls', panel.id);
    const closer = panel.querySelector('header .close');
    const search = panel.querySelector('input.ptoc-search');
    search.setAttribute('aria-label', 'Search table of contents');

    for (const [control, label] of [[opener, 'Open table of contents'], [closer, 'Close table of contents']]) {
        control.setAttribute('role', 'button');
        control.setAttribute('aria-label', label);
        control.tabIndex = 0;
        // Activate the upstream click handler, without scrolling on Space.
        control.addEventListener('keydown', event => {
            if (event.key === 'Enter' || event.key === ' ') {
                event.preventDefault();
                if (!event.repeat) control.click();
            }
        });
        // Upstream also uses Space keyup to close the panel; don't undo an opening keystroke.
        control.addEventListener('keyup', event => {
            if (event.key === 'Enter' || event.key === ' ') event.stopPropagation();
        });
    }

    // Upstream signals panel state by its inline width. Observe that state rather than
    // duplicating its click/link logic, including the "keep open" checkbox behavior.
    const syncPanel = () => {
        const open = panel.style.width !== '' && panel.style.width !== '0px';
        const wasOpen = opener.getAttribute('aria-expanded') === 'true';
        if (!open && panel.contains(document.activeElement)) opener.focus();
        panel.inert = !open;
        opener.setAttribute('aria-expanded', String(open));
        if (open && !wasOpen) search.focus({preventScroll: true});
    };
    syncPanel();
    new MutationObserver(syncPanel).observe(panel, {attributes: true, attributeFilter: ['style']});
});
