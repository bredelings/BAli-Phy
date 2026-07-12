'use strict';

const assert = require('node:assert/strict');

const {
    createScale,
    createDivergingScale,
    automaticScaleBounds,
    canUseLogScale,
    preferredTransform,
    paletteColor,
    paletteGradient,
    blendWithWhite,
} = require('../../../src/tools/alignment-draw.js');

// Compares floating-point scale results without coupling the tests to their
// exact arithmetic evaluation order.
function assertClose(actual, expected, message)
{
    const tolerance = 1e-12;
    assert.ok(Math.abs(actual - expected) <= tolerance,
              `${message}: expected ${expected}, got ${actual}`);
}

{
    assert.deepEqual(paletteColor(0, 'blue-red'), [8, 48, 107],
                     'the sequential blue-red palette starts at blue');
    assert.deepEqual(paletteColor(1, 'blue-red'), [239, 59, 44],
                     'the sequential blue-red palette ends at red');
    assert.deepEqual(paletteColor(0.5, 'blue-gray-red'), [232, 232, 232],
                     'the diverging palette has a light-gray midpoint');
    assert.match(paletteGradient('blue-gray-red'), /rgb\(232, 232, 232\) 50%/,
                 'the legend gradient contains the same gray midpoint');
    assert.throws(() => paletteColor(0.5, 'missing'), /unknown property palette/i,
                  'stale palette state is rejected rather than silently miscolored');
}

{
    const base = createScale([0, 2, 10], {
        transform: 'linear',
        lower: 0,
        upper: 10,
    });
    const scale = createDivergingScale(base, 2);
    assert.equal(scale.normalize(0), 0, 'the robust lower bound remains blue');
    assert.equal(scale.normalize(2), 0.5, 'the raw median maps to gray');
    assert.equal(scale.normalize(10), 1, 'the robust upper bound remains red');
    assertClose(scale.normalize(1), 0.25,
                'the lower half is normalized independently');
    assertClose(scale.normalize(6), 0.75,
                'the upper half is normalized independently');
    const ticks = scale.legendTicks(5);
    assert.deepEqual(ticks.map((tick) => tick.position), [0, 0.25, 0.5, 0.75, 1]);
    assertClose(ticks[2].value, 2, 'the central legend tick reports the median');
}

{
    const base = createScale([0, 0.01, 0.1, 10], {
        transform: 'log10',
        lower: 0.01,
        upper: 10,
    });
    const scale = createDivergingScale(base, 0.1);
    assert.equal(scale.normalize(0), 0,
                 'zero remains at the clipped floor of a diverging log scale');
    assert.equal(scale.normalize(0.1), 0.5,
                 'a log-scale median still maps exactly to gray');
    assert.equal(scale.normalize(10), 1,
                 'the logarithmic upper bound remains red');
}

{
    const base = createScale([0, 1, 2], {
        transform: 'linear',
        lower: 0,
        upper: 2,
    });
    const scale = createDivergingScale(base, 0);
    assert.equal(scale.normalize(0), 0.5,
                 'a median at the lower bound collapses the empty blue half');
    const ticks = scale.legendTicks(3);
    assert.deepEqual(ticks.map((tick) => tick.position), [0.5, 0.75, 1],
                     'a collapsed half does not invent lower-tail legend values');
    for (const tick of ticks)
        assertClose(scale.normalize(tick.value), tick.position,
                    'degenerate diverging legend labels still invert the scale');
}

{
    const scale = createScale([0, 2.5, 10], {
        transform: 'linear',
        lower: 0,
        upper: 10,
    });

    assert.equal(scale.normalize(-1), 0, 'linear scales clip below the lower bound');
    assertClose(scale.normalize(2.5), 0.25, 'linear scales preserve relative position');
    assert.equal(scale.normalize(20), 1, 'linear scales clip above the upper bound');

    const ticks = scale.legendTicks(3);
    assert.deepEqual(ticks.map((tick) => tick.position), [0, 0.5, 1]);
    assert.deepEqual(ticks.map((tick) => tick.value), [0, 5, 10]);
}

{
    const scale = createScale([0.1, 1, 10], {
        transform: 'log10',
        lower: 0.1,
        upper: 10,
    });

    assert.equal(scale.normalize(0.01), 0, 'log scales clip below the lower bound');
    assertClose(scale.normalize(1), 0.5, 'log scales give equal space to equal ratios');
    assert.equal(scale.normalize(100), 1, 'log scales clip above the upper bound');

    const ticks = scale.legendTicks(3);
    assert.deepEqual(ticks.map((tick) => tick.position), [0, 0.5, 1]);
    assert.deepEqual(ticks.map((tick) => tick.value), [0.1, 1, 10]);

    const zeroScale = createScale([0, 0.1, 1], {
        transform: 'log10',
        lower: 0.1,
        upper: 1,
    });
    assert.equal(zeroScale.normalize(0), 0,
                 'zero values clip to the positive logarithmic floor');
    const automaticZeroScale = createScale([0, 0.1, 1], {transform: 'log10'});
    assert.equal(automaticZeroScale.lower, 0.1,
                 'an implicit logarithmic floor uses the smallest positive value');
    assert.throws(() => createScale([-1, 1], {transform: 'log10'}), /negative/i);
}

{
    assert.deepEqual(automaticScaleBounds([0, 0.1, 1, 10], 'log10', 'full'),
                     {lower: 0.1, upper: 10},
                     'logarithmic full bounds use the positive extent');
    assert.equal(canUseLogScale('rate', [0, 1, 2]), true,
                 'zero rates permit a clipped logarithmic scale');
    assert.equal(canUseLogScale('rate', [-1, 1, 2]), false,
                 'negative properties do not permit logarithmic display');
    assert.equal(canUseLogScale('posSelection', [0.01, 0.5, 0.99]), false,
                 'the built-in probability property remains linear-only');
    assert.equal(preferredTransform('rate', []), 'linear',
                 'properties without finite values retain a usable default state');
}

{
    const uniform = Array.from({length: 100}, (_, index) => index / 99);
    const logUniform = [0, ...Array.from({length: 100}, (_, index) =>
        10 ** (-3 + (3 * index) / 99))];
    assert.equal(preferredTransform('dNdS', uniform), 'linear',
                 'uniform ratios retain a linear default');
    assert.equal(preferredTransform('rate', logUniform), 'log10',
                 'orders-of-magnitude rates default to logarithmic display');
    assert.equal(preferredTransform('posSelection', logUniform), 'linear',
                 'probabilities do not inherit a distribution-driven log default');
    assert.equal(preferredTransform('rate', logUniform.slice(0, 10)), 'linear',
                 'small samples retain the conservative linear default');
}

{
    const scale = createScale([1, 2, 2, 4], {transform: 'rank'});

    assert.equal(scale.normalize(1), 0, 'rank scales place the minimum at zero');
    assertClose(scale.normalize(2), 0.5, 'rank scales assign tied values their midrank');
    assert.equal(scale.normalize(4), 1, 'rank scales place the maximum at one');

    const ticks = scale.legendTicks(3);
    assert.deepEqual(ticks.map((tick) => tick.position), [0, 0.5, 1]);
    assert.deepEqual(ticks.map((tick) => tick.value), [1, 2, 4]);
}

{
    const scale = createScale([7, 7, 7], {transform: 'linear'});
    assertClose(scale.normalize(7), 0.5, 'constant properties use the palette midpoint');
    assert.deepEqual(scale.legendTicks(5), [{position: 0.5, value: 7}],
                     'constant properties have one centered legend tick');
}

{
    const scale = createScale([1, 1, 2, 4], {transform: 'rank'});
    for (const tick of scale.legendTicks(5))
        assertClose(scale.normalize(tick.value), tick.position,
                    'rank legend labels invert the tied-rank mapping');
}

{
    assert.throws(() => createScale([1, 2], {lower: 3, upper: 2}), /lower/i,
                  'custom bounds must be ordered');
}

{
    const values = Array.from({length: 200000}, (_, index) => index + 1);
    const scale = createScale(values, {transform: 'linear'});
    assert.equal(scale.lower, 1, 'large scales find their minimum without argument expansion');
    assert.equal(scale.upper, 200000, 'large scales find their maximum without argument expansion');
}

{
    const color = [0, 100, 200];
    assert.deepEqual(blendWithWhite(color, 0), [255, 255, 255],
                     'zero AU certainty removes the property color');
    assert.deepEqual(blendWithWhite(color, 0.5), [128, 178, 228],
                     'intermediate AU certainty blends toward white');
    assert.deepEqual(blendWithWhite(color, 1), color,
                     'full AU certainty preserves the property color');
    assert.throws(() => blendWithWhite(color, Number.NaN), /finite/i,
                  'non-finite AU certainty is rejected');
}
