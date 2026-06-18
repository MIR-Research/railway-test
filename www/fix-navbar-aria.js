// www/fix-navbar-aria.js
(function () {
  // ---------- utils ----------
  function debounce(fn, wait) {
    let t; return function () { clearTimeout(t); t = setTimeout(() => fn.apply(this, arguments), wait); };
  }
  function getTargetId(link) {
    const ds = link.getAttribute('data-bs-target') || link.getAttribute('data-target');
    if (ds && ds.startsWith('#')) return ds.slice(1);
    const href = link.getAttribute('href');
    if (href && href.startsWith('#') && href.length > 1) return href.slice(1);
    return null;
  }
  function ensureId(el, prefix) {
    if (!el.id) el.id = prefix + Math.random().toString(36).slice(2, 9);
    return el.id;
  }

  // ---------- TOP NAVBAR: treat as navigation, not tablist ----------
  function fixTopNavbar() {
    const nav = document.getElementById('page_navbar');
    if (!nav) return;

    nav.setAttribute('role', 'list');                // <-- keep native list semantics
    nav.setAttribute('aria-label', 'Primary navigation');
    
    if (nav.getAttribute('role') === 'tablist') {
      nav.setAttribute('role', 'list');              // <-- force back to list if reset
    }
    

    // Neutralize live region inside navbar (your page_indicator)
    const live = nav.querySelector('[aria-live]');
    if (live) { live.removeAttribute('aria-live'); live.setAttribute('aria-hidden', 'true'); }

    // Any “tab” roles inside the top navbar are wrong — strip them
    nav.querySelectorAll('[role="tab"], [role="presentation"]').forEach(el => el.removeAttribute('role'));
    nav.querySelectorAll('.nav-link, .dropdown-toggle').forEach(a => {
      a.removeAttribute('aria-selected');
      a.removeAttribute('tabindex');
      a.removeAttribute('aria-controls');
    });

    // Dropdown toggles: proper ARIA without pretending to be tabs
    nav.querySelectorAll('[data-bs-toggle="dropdown"], [data-toggle="dropdown"]').forEach(tog => {
      tog.setAttribute('role', 'button');
      tog.setAttribute('aria-haspopup', 'true');
      const menu = tog.parentElement && tog.parentElement.querySelector('.dropdown-menu');
      if (menu) {
        ensureId(menu, 'ddmenu-');
        tog.setAttribute('aria-controls', menu.id);
      }
      tog.setAttribute('aria-expanded', tog.classList.contains('show') ? 'true' : 'false');
    });

    // Keep aria-expanded in sync with Bootstrap
    document.addEventListener('shown.bs.dropdown', e => {
      const dd = e.target.closest('.dropdown');
      if (dd) dd.querySelectorAll('.dropdown-toggle').forEach(t => t.setAttribute('aria-expanded', 'true'));
    });
    document.addEventListener('hidden.bs.dropdown', e => {
      const dd = e.target.closest('.dropdown');
      if (dd) dd.querySelectorAll('.dropdown-toggle').forEach(t => t.setAttribute('aria-expanded', 'false'));
    });
  }

  // ---------- REAL TABSETS (NOT the top navbar) ----------
  function fixTabsets() {
    // Select any .nav that actually behaves like tabs (has tab toggles), excluding #page_navbar
    const candidates = Array.from(document.querySelectorAll('.nav'))
      .filter(nav =>
        nav.id !== 'page_navbar' &&
        !nav.closest('#page_navbar') &&
        nav.querySelector('[data-bs-toggle="tab"], [data-toggle="tab"]')
      );

    candidates.forEach(tablist => {
      tablist.setAttribute('role', 'tablist');
      tablist.querySelectorAll(':scope > li').forEach(li => li.setAttribute('role', 'presentation'));


      tablist.querySelectorAll('.nav-link, a').forEach(link => {
        if (!link.matches('[data-bs-toggle="tab"], [data-toggle="tab"]')) return;

        // Tabs
        link.setAttribute('role', 'tab');

        // Selected / focusability based on .active
        const isActive = link.classList.contains('active') || link.getAttribute('aria-selected') === 'true';
        link.setAttribute('aria-selected', isActive ? 'true' : 'false');
        link.setAttribute('tabindex', isActive ? '0' : '-1');

        // Relationships
        const tid = getTargetId(link);
        if (tid) {
          link.setAttribute('aria-controls', tid);
          const panel = document.getElementById(tid);
          if (panel) {
            panel.setAttribute('role', 'tabpanel');
            const tabId = ensureId(link, 'tab-');
            panel.setAttribute('aria-labelledby', tabId);
            panel.setAttribute('aria-hidden', isActive ? 'false' : 'true');
          }
        }
      });
    });

    // When Bootstrap switches tabs, sync ARIA state
    document.addEventListener('shown.bs.tab', function (e) {
      const newTab = e.target;         // .nav-link becoming active
      const oldTab = e.relatedTarget;  // previously active (may be null)

      [newTab, oldTab].forEach(tab => {
        if (!tab) return;
        const insideTopNav = !!tab.closest('#page_navbar');
        if (insideTopNav) return; // never mark top navbar as tabs

        const selected = tab === newTab;
        tab.setAttribute('aria-selected', selected ? 'true' : 'false');
        tab.setAttribute('tabindex',      selected ? '0'    : '-1');

        const tid = getTargetId(tab);
        const panel = tid ? document.getElementById(tid) : null;
        if (panel) panel.setAttribute('aria-hidden', selected ? 'false' : 'true');

        // Ensure parent has role=tablist
        const parentNav = tab.closest('.nav');
        if (parentNav && !parentNav.closest('#page_navbar')) {
          parentNav.setAttribute('role', 'tablist');
        }
      });
    });
  }

  // If you want Tools dropdown items to explicitly tell Shiny which page:
  function wireToolsClicks() {
    document.addEventListener('click', function (e) {
      const item = e.target.closest('.dropdown-menu .dropdown-item');
      if (!item) return;
      const val = item.getAttribute('data-value') || (window.jQuery && window.jQuery(item).data('value'));
      if (val && window.Shiny && Shiny.setInputValue) {
        Shiny.setInputValue('page_navbar', val, { priority: 'event' });
      }
    });
  }

  const repair = debounce(function () {
    fixTopNavbar();
    fixTabsets();
  }, 20);

  // Run now + after Shiny/DOM changes
  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', repair);
  } else {
    repair();
  }
  if (window.jQuery) {
    jQuery(document).on('shiny:recalculated', repair);
  }
  const mo = new MutationObserver(repair);
  mo.observe(document.documentElement, { childList: true, subtree: true, attributes: true });

  wireToolsClicks();
})();
