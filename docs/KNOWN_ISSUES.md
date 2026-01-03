# Known Issues and Future Improvements

This document tracks known issues and planned improvements for the interactive Explore dashboard.

## 1. Data File Size

**Issue:** The generated JSON data files total approximately 455MB across all historical nowcast dates, which is large for a GitHub-hosted data branch.

**Current state:**
- 3,276 forecast files (~50KB each)
- 6,344 target data files (~10KB each)
- Total: ~455MB uncompressed

**Investigated solutions:**

| Approach | Size Reduction | Notes |
|----------|---------------|-------|
| Gzip compression | ~87% (455MB → ~60MB) | Requires server-side decompression or browser handling |
| Remove multinomial PI columns | ~43% (455MB → 260MB) | Breaks prediction interval display (see below) |
| Reduce decimal precision (2 digits) | ~63% (455MB → 168MB) | Minimal visual impact |
| Combined (no multinomial + 2 digits + gzip) | ~97% (455MB → ~13MB) | Most aggressive reduction |

**Complication with multinomial columns:**
The prediction interval display relies on `multinomial_q*` columns for "prediction intervals" vs `q*` columns for "confidence intervals". The JavaScript code checks `type === "prediction"` and uses the appropriate prefix. Removing multinomial columns breaks prediction interval display unless the frontend logic is also updated.

**Recommended path forward:**
1. Keep multinomial columns for now (prediction intervals are valuable)
2. Implement gzip compression at the pipeline level
3. Update frontend to handle `.json.gz` files with browser decompression
4. Consider reducing decimal precision to 3-4 digits (minimal visual impact)

---

## 2. Model/Clade Selection Persistence Across Dates

**Issue:** When navigating to a nowcast date where a previously-selected model or clade doesn't exist, that selection is lost. Navigating back to the original date does not restore the selection.

**Current behavior:**
- Model checkboxes are rebuilt based on `availableModels` (from current forecast data)
- Clade checkboxes are rebuilt from `config.clades_by_date[selectedNowcastDate]`
- Selections are stored in `window._dashboardState` but may be filtered to only include available items

**Desired behavior:**
- User's selection intent should persist regardless of availability
- When navigating back to a date where a model/clade exists, it should still be selected

**Potential solutions:**

1. **Store full selection state, never filter:** Don't remove items from saved selections when they become unavailable. When rendering, check against both saved state AND availability.

2. **Show unavailable items as disabled but checked:** Build checkbox lists from the union of all possible models/clades, showing unavailable items as disabled with their checked state preserved.

3. **Ghost selections:** Show unavailable-but-selected items in a separate grayed-out section.

**Recommended approach:**
Option 1 is simplest - modify the checkbox rendering logic to:
- Never remove items from `window._dashboardState.*Selections` when unavailable
- When rendering, mark item as checked if it's in saved selections AND available
- This preserves intent without UI complexity

---

## 3. URL Permanence / Deep Linking

**Issue:** The dashboard state (selected location, date, models, clades, intervals) is not reflected in the URL. Users cannot share links to specific views or bookmark particular configurations.

**Current behavior:**
- All state is stored in JavaScript memory (`window._dashboardState`)
- Refreshing the page resets to defaults
- No way to share a specific view with others

**Desired behavior:**
- URL query parameters reflect current selections
- Loading a URL with parameters restores that exact view
- Changing selections updates the URL (without page reload)

**Implementation approach:**

1. **URL parameter schema:**
   ```
   /explore.html?loc=MA&date=2025-12-24&models=hub-ensemble,UMass-trends&clades=24A,24B&interval=80&type=confidence&data=round-open,latest
   ```

2. **On page load:**
   - Parse URL parameters
   - Use parameter values as initial selections (override defaults)
   - Fall back to defaults for missing parameters

3. **On selection change:**
   - Update URL using `history.replaceState()` (no page reload)
   - Debounce updates to avoid excessive history entries

4. **Observable JS implementation:**
   ```javascript
   // Read from URL on init
   const urlParams = new URLSearchParams(window.location.search);
   const initialLocation = urlParams.get('loc') || 'CA';

   // Update URL on change
   function updateURL(state) {
     const params = new URLSearchParams();
     params.set('loc', state.location);
     params.set('date', state.date);
     // ... other params
     history.replaceState({}, '', `?${params.toString()}`);
   }
   ```

**Considerations:**
- Keep parameter names short to avoid overly long URLs
- Handle invalid/outdated parameter values gracefully
- Consider encoding model/clade lists efficiently (comma-separated vs JSON)

---

## Priority Ranking

1. **URL Permanence** - High impact for usability and sharing
2. **Selection Persistence** - Medium impact, improves UX when exploring
3. **File Size** - Lower priority until data branch size becomes problematic

---

*Last updated: 2025-01-03*
