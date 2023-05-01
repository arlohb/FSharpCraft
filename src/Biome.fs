module Biome

open DotnetNoise

open Block

type Biome =
    | Grasslands
    | Mountains

let biomes = [ Grasslands; Mountains ]

let biomeNoise =
    let noise = new FastNoise()
    noise.Frequency <- 0.01f
    noise

let getBiome x y =
    let offsetScale = 25f
    let offsetFreq = 2.5f

    let offsetX =
        biomeNoise.GetSimplexFractal(float32 x * offsetFreq, float32 y * offsetFreq)
        * offsetScale

    let offsetY =
        biomeNoise.GetSimplexFractal(float32 x * offsetFreq + 100f, float32 y * offsetFreq + 100f)
        * offsetScale

    // Mapped to 0..1
    biomeNoise.GetCellular(float32 x + offsetX, float32 y + offsetY) + 1f / 2f
    // Map to 0 .. biome count
    |> ((*) (float32 biomes.Length))
    // Get index
    |> floor
    // Fix edge cases
    |> (fun i -> if i = float32 biomes.Length then i - 1f else i)
    |> (fun i -> if i < 0f then 0f else i)
    // Get biome
    |> (fun i -> biomes[int i])

// Octaves:
//   How many noise 'passes' to add together.
//   Default: 3
// Frequency:
//   The xy scale of the noise.
//   Default: 0.01
// Lacunarity:
//   A cumulative freq. modifier for each octave
//   Default: 2
// Gain:
//   A cumulative amplitude modifier for each octave
//   Default: 0.05
let grasslandsNoise =
    let noise = new FastNoise()
    noise.Octaves <- 4
    noise.Frequency <- 0.05f
    noise.Lacunarity <- 1.5f
    noise.Gain <- 0.01f
    noise

let mountainsNoise =
    let noise = new FastNoise()
    noise.Octaves <- 4
    noise.Frequency <- 0.02f
    noise.Lacunarity <- 2f
    noise.Gain <- 0.1f
    noise

let biomeWorldGen biome x y z =
    let randomOffset = 56478f

    match biome with
    | Grasslands ->
        let height = grasslandsNoise.GetSimplexFractal(float32 x, float32 y) * 5f

        if float32 z < height - 3f then
            Stone
        elif float32 z <= height then
            let randomValue =
                grasslandsNoise.GetSimplexFractal(float32 x * 2f + randomOffset, float32 y * 2f + randomOffset)

            if randomValue > 0.3f then Stone else Dirt
        else
            Air
    | Mountains ->
        let noise = mountainsNoise.GetSimplexFractal(float32 x, float32 y) |> abs
        let height = (1f - noise) * 40f - 40f

        if float32 z < height - 3f then
            Stone
        elif float32 z <= height then
            let randomValue =
                mountainsNoise.GetSimplexFractal(float32 x * 2f + randomOffset, float32 y * 2f + randomOffset)

            if randomValue > 0.3f then Dirt else Stone
        else
            Air
