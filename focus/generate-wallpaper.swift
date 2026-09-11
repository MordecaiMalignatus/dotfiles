// Generates the focus-mode wallpaper: a low-contrast graphite triangle grid.
// Run once and commit the output: swift focus/generate-wallpaper.swift focus/wallpaper.png
import Foundation
import CoreGraphics
import ImageIO

let width = 3840
let height = 2160
let cell = 320
let base = (r: 0.13, g: 0.145, b: 0.175)

guard CommandLine.arguments.count == 2 else {
    FileHandle.standardError.write("usage: swift generate-wallpaper.swift <output.png>\n".data(using: .utf8)!)
    exit(2)
}
let outputPath = CommandLine.arguments[1]

let ctx = CGContext(data: nil, width: width, height: height,
                    bitsPerComponent: 8, bytesPerRow: 0,
                    space: CGColorSpace(name: CGColorSpace.sRGB)!,
                    bitmapInfo: CGImageAlphaInfo.premultipliedLast.rawValue)!

srand48(42) // deterministic, so regeneration reproduces the committed image

func fill(_ points: [CGPoint]) {
    let delta = drand48() * 0.055 - 0.02
    ctx.setFillColor(CGColor(srgbRed: base.r + delta, green: base.g + delta,
                             blue: base.b + delta, alpha: 1))
    let path = CGMutablePath()
    path.addLines(between: points)
    path.closeSubpath()
    ctx.addPath(path)
    ctx.fillPath()
}

for row in 0..<(height / cell + 1) {
    for col in 0..<(width / cell) {
        let x = CGFloat(col * cell), y = CGFloat(row * cell), s = CGFloat(cell)
        let tl = CGPoint(x: x, y: y), tr = CGPoint(x: x + s, y: y)
        let bl = CGPoint(x: x, y: y + s), br = CGPoint(x: x + s, y: y + s)
        if (col + row) % 2 == 0 {
            fill([tl, tr, br])
            fill([tl, br, bl])
        } else {
            fill([tr, bl, tl])
            fill([tr, br, bl])
        }
    }
}

let url = URL(fileURLWithPath: outputPath) as CFURL
let dest = CGImageDestinationCreateWithURL(url, "public.png" as CFString, 1, nil)!
CGImageDestinationAddImage(dest, ctx.makeImage()!, nil)
guard CGImageDestinationFinalize(dest) else {
    FileHandle.standardError.write("failed to write \(outputPath)\n".data(using: .utf8)!)
    exit(1)
}
