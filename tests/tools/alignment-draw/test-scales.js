'use strict';

const assert = require('node:assert/strict');

const {
    createScale,
    automaticScaleBounds,
    canUseLogScale,
    preferredTransform,
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
