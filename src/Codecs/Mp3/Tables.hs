--
-- module Tables - Tables defined in the specification.
--
-- This code is part of the Experimental Haskell MP3 Decoder, version 0.0.1.
-- Copyright (c) 2008 Bjorn Edstrom <be@bjrn.se>
--
-- This software is provided 'as-is', without any express or implied
-- warranty. In no event will the authors be held liable for any damages
-- arising from the use of this software.
--
-- Permission is granted to anyone to use this software for any purpose,
-- including commercial applications, and to alter it and redistribute it
-- freely, subject to the following restrictions:
--
--    1. The origin of this software must not be misrepresented; you must not
--    claim that you wrote the original software. If you use this software
--    in a product, an acknowledgment in the product documentation would be
--    appreciated but is not required.
--
--    2. Altered source versions must be plainly marked as such, and must not be
--    misrepresented as being the original software.
--
--    3. This notice may not be removed or altered from any source
--    distribution.
--
-- TODO: Change all tables to something more suitable with O(1) lookup.
--
module Codecs.Mp3.Tables (
     tableImdctWindow
    ,tableSlen
    ,tableReorder2 -- See below.
    ,tablePretab


) where

--
-- tableImdctWindow
--
-- Window coefficients for IMDCT transform.
-- 
tableImdctWindow :: Floating a => Int -> [a]
tableImdctWindow blocktype
    | blocktype == 0   = [coeff n 36 0.5     | n <- [ 0..35]]
    | blocktype == 1   = [coeff n 36 0.5     | n <- [ 0..17]] ++
                         [1.0                | n <- [18..23]] ++
                         [coeff n 12 (-17.5) | n <- [24..29]] ++
                         [0.0                | n <- [30..35]]
    | blocktype == 2   = [coeff n 12 0.5     | n <- [ 0..11]] ++
                         [0.0                | n <- [12..35]]
    | blocktype == 3   = [0.0                | n <- [ 0.. 5]] ++
                         [coeff n 12 (-5.5)  | n <- [ 6..11]] ++
                         [1.0                | n <- [12..17]] ++
                         [coeff n 36 0.5     | n <- [18..35]]
    | otherwise        = error "Wrong blocktype."
    where
        coeff n div' add' = sin (pi/div' * ((fromIntegral n) + add'))

-- 
-- tablePretab
--
-- Modifies certain scale factors for higher range than 4 bits.
--
tablePretab :: [Int]
tablePretab = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
               1, 1, 1, 1, 2, 2, 3, 3, 3, 2, 0, 0]


--
-- tableSlen
--
-- This table is used for parsing the scale factor bands.
--
tableSlen :: [(Int, Int)]
tableSlen = [(0,0), (0,1), (0,2), (0,3), (3,0), (1,1), (1,2), (1,3),
             (2,1), (2,2), (2,3), (3,1) ,(3,2), (3,3), (4,2), (4,3)] 


--
-- tableReorder2
--
-- This table does only one reordering. If we do this, we have to
-- imdct 6 [f!!0,f!!3,f!!6,f!!9,f!!12,f!!15] (see Decoder.hs)
--
tableReorder2 :: Int -> [Int]
tableReorder2 44100 = tab1
tableReorder2 48000 = tab2
tableReorder2 32000 = tab3
tableReorder2 _     = error "Wrong SR for Table."

-- The scale factor bandwidth for the first band in a short 192-granule is 4,
-- so the first 12 samples are the first 4 samples, interleaved.
tab1 :: [Int]
tab1 = [0, 4, 8, 1, 5, 9, 2, 6, 10, 3, 7, 11,
        12, 16, 20, 13,
        17, 21, 14, 18, 22, 15, 19, 23, 24, 28, 32, 25, 29, 33, 26,
        30, 34, 27, 31, 35, 36, 40, 44, 37, 41, 45, 38, 42, 46, 39,
        43, 47, 48, 54, 60, 49, 55, 61, 50, 56, 62, 51, 57, 63, 52,
        58, 64, 53, 59, 65, 66, 74, 82, 67, 75, 83, 68, 76, 84, 69,
        77, 85, 70, 78, 86, 71, 79, 87, 72, 80, 88, 73, 81, 89, 90,
        100, 110, 91, 101, 111, 92, 102, 112, 93, 103, 113, 94, 104, 114, 95,
        105, 115, 96, 106, 116, 97, 107, 117, 98, 108, 118, 99, 109, 119, 120,
        132, 144, 121, 133, 145, 122, 134, 146, 123, 135, 147, 124, 136, 148, 125,
        137, 149, 126, 138, 150, 127, 139, 151, 128, 140, 152, 129, 141, 153, 130,
        142, 154, 131, 143, 155, 156, 170, 184, 157, 171, 185, 158, 172, 186, 159,
        173, 187, 160, 174, 188, 161, 175, 189, 162, 176, 190, 163, 177, 191, 164,
        178, 192, 165, 179, 193, 166, 180, 194, 167, 181, 195, 168, 182, 196, 169,
        183, 197, 198, 216, 234, 199, 217, 235, 200, 218, 236, 201, 219, 237, 202,
        220, 238, 203, 221, 239, 204, 222, 240, 205, 223, 241, 206, 224, 242, 207,
        225, 243, 208, 226, 244, 209, 227, 245, 210, 228, 246, 211, 229, 247, 212,
        230, 248, 213, 231, 249, 214, 232, 250, 215, 233, 251, 252, 274, 296, 253,
        275, 297, 254, 276, 298, 255, 277, 299, 256, 278, 300, 257, 279, 301, 258,
        280, 302, 259, 281, 303, 260, 282, 304, 261, 283, 305, 262, 284, 306, 263,
        285, 307, 264, 286, 308, 265, 287, 309, 266, 288, 310, 267, 289, 311, 268,
        290, 312, 269, 291, 313, 270, 292, 314, 271, 293, 315, 272, 294, 316, 273,
        295, 317, 318, 348, 378, 319, 349, 379, 320, 350, 380, 321, 351, 381, 322,
        352, 382, 323, 353, 383, 324, 354, 384, 325, 355, 385, 326, 356, 386, 327,
        357, 387, 328, 358, 388, 329, 359, 389, 330, 360, 390, 331, 361, 391, 332,
        362, 392, 333, 363, 393, 334, 364, 394, 335, 365, 395, 336, 366, 396, 337,
        367, 397, 338, 368, 398, 339, 369, 399, 340, 370, 400, 341, 371, 401, 342,
        372, 402, 343, 373, 403, 344, 374, 404, 345, 375, 405, 346, 376, 406, 347,
        377, 407, 408, 464, 520, 409, 465, 521, 410, 466, 522, 411, 467, 523, 412,
        468, 524, 413, 469, 525, 414, 470, 526, 415, 471, 527, 416, 472, 528, 417,
        473, 529, 418, 474, 530, 419, 475, 531, 420, 476, 532, 421, 477, 533, 422,
        478, 534, 423, 479, 535, 424, 480, 536, 425, 481, 537, 426, 482, 538, 427,
        483, 539, 428, 484, 540, 429, 485, 541, 430, 486, 542, 431, 487, 543, 432,
        488, 544, 433, 489, 545, 434, 490, 546, 435, 491, 547, 436, 492, 548, 437,
        493, 549, 438, 494, 550, 439, 495, 551, 440, 496, 552, 441, 497, 553, 442,
        498, 554, 443, 499, 555, 444, 500, 556, 445, 501, 557, 446, 502, 558, 447,
        503, 559, 448, 504, 560, 449, 505, 561, 450, 506, 562, 451, 507, 563, 452,
        508, 564, 453, 509, 565, 454, 510, 566, 455, 511, 567, 456, 512, 568, 457,
        513, 569, 458, 514, 570, 459, 515, 571, 460, 516, 572, 461, 517, 573, 462,
        518, 574, 463, 519, 575]

tab2 :: [Int]
tab2 = [0, 4, 8, 1, 5, 9, 2, 6, 10, 3, 7, 11, 12, 16, 20, 13,
        17, 21, 14, 18, 22, 15, 19, 23, 24, 28, 32, 25, 29, 33, 26,
        30, 34, 27, 31, 35, 36, 40, 44, 37, 41, 45, 38, 42, 46, 39,
        43, 47, 48, 54, 60, 49, 55, 61, 50, 56, 62, 51, 57, 63, 52,
        58, 64, 53, 59, 65, 66, 72, 78, 67, 73, 79, 68, 74, 80, 69,
        75, 81, 70, 76, 82, 71, 77, 83, 84, 94, 104, 85, 95, 105, 86,
        96, 106, 87, 97, 107, 88, 98, 108, 89, 99, 109, 90, 100, 110, 91,
        101, 111, 92, 102, 112, 93, 103, 113, 114, 126, 138, 115, 127, 139, 116,
        128, 140, 117, 129, 141, 118, 130, 142, 119, 131, 143, 120, 132, 144, 121,
        133, 145, 122, 134, 146, 123, 135, 147, 124, 136, 148, 125, 137, 149, 150,
        164, 178, 151, 165, 179, 152, 166, 180, 153, 167, 181, 154, 168, 182, 155,
        169, 183, 156, 170, 184, 157, 171, 185, 158, 172, 186, 159, 173, 187, 160,
        174, 188, 161, 175, 189, 162, 176, 190, 163, 177, 191, 192, 208, 224, 193,
        209, 225, 194, 210, 226, 195, 211, 227, 196, 212, 228, 197, 213, 229, 198,
        214, 230, 199, 215, 231, 200, 216, 232, 201, 217, 233, 202, 218, 234, 203,
        219, 235, 204, 220, 236, 205, 221, 237, 206, 222, 238, 207, 223, 239, 240,
        260, 280, 241, 261, 281, 242, 262, 282, 243, 263, 283, 244, 264, 284, 245,
        265, 285, 246, 266, 286, 247, 267, 287, 248, 268, 288, 249, 269, 289, 250,
        270, 290, 251, 271, 291, 252, 272, 292, 253, 273, 293, 254, 274, 294, 255,
        275, 295, 256, 276, 296, 257, 277, 297, 258, 278, 298, 259, 279, 299, 300,
        326, 352, 301, 327, 353, 302, 328, 354, 303, 329, 355, 304, 330, 356, 305,
        331, 357, 306, 332, 358, 307, 333, 359, 308, 334, 360, 309, 335, 361, 310,
        336, 362, 311, 337, 363, 312, 338, 364, 313, 339, 365, 314, 340, 366, 315,
        341, 367, 316, 342, 368, 317, 343, 369, 318, 344, 370, 319, 345, 371, 320,
        346, 372, 321, 347, 373, 322, 348, 374, 323, 349, 375, 324, 350, 376, 325,
        351, 377, 378, 444, 510, 379, 445, 511, 380, 446, 512, 381, 447, 513, 382,
        448, 514, 383, 449, 515, 384, 450, 516, 385, 451, 517, 386, 452, 518, 387,
        453, 519, 388, 454, 520, 389, 455, 521, 390, 456, 522, 391, 457, 523, 392,
        458, 524, 393, 459, 525, 394, 460, 526, 395, 461, 527, 396, 462, 528, 397,
        463, 529, 398, 464, 530, 399, 465, 531, 400, 466, 532, 401, 467, 533, 402,
        468, 534, 403, 469, 535, 404, 470, 536, 405, 471, 537, 406, 472, 538, 407,
        473, 539, 408, 474, 540, 409, 475, 541, 410, 476, 542, 411, 477, 543, 412,
        478, 544, 413, 479, 545, 414, 480, 546, 415, 481, 547, 416, 482, 548, 417,
        483, 549, 418, 484, 550, 419, 485, 551, 420, 486, 552, 421, 487, 553, 422,
        488, 554, 423, 489, 555, 424, 490, 556, 425, 491, 557, 426, 492, 558, 427,
        493, 559, 428, 494, 560, 429, 495, 561, 430, 496, 562, 431, 497, 563, 432,
        498, 564, 433, 499, 565, 434, 500, 566, 435, 501, 567, 436, 502, 568, 437,
        503, 569, 438, 504, 570, 439, 505, 571, 440, 506, 572, 441, 507, 573, 442,
        508, 574, 443, 509, 575]

tab3 :: [Int]
tab3 =  [0, 4, 8, 1, 5, 9, 2, 6, 10, 3, 7, 11, 12, 16, 20, 13,
         17, 21, 14, 18, 22, 15, 19, 23, 24, 28, 32, 25, 29, 33, 26,
         30, 34, 27, 31, 35, 36, 40, 44, 37, 41, 45, 38, 42, 46, 39,
         43, 47, 48, 54, 60, 49, 55, 61, 50, 56, 62, 51, 57, 63, 52,
         58, 64, 53, 59, 65, 66, 74, 82, 67, 75, 83, 68, 76, 84, 69,
         77, 85, 70, 78, 86, 71, 79, 87, 72, 80, 88, 73, 81, 89, 90,
         102, 114, 91, 103, 115, 92, 104, 116, 93, 105, 117, 94, 106, 118, 95,
         107, 119, 96, 108, 120, 97, 109, 121, 98, 110, 122, 99, 111, 123, 100,
         112, 124, 101, 113, 125, 126, 142, 158, 127, 143, 159, 128, 144, 160, 129,
         145, 161, 130, 146, 162, 131, 147, 163, 132, 148, 164, 133, 149, 165, 134,
         150, 166, 135, 151, 167, 136, 152, 168, 137, 153, 169, 138, 154, 170, 139,
         155, 171, 140, 156, 172, 141, 157, 173, 174, 194, 214, 175, 195, 215, 176,
         196, 216, 177, 197, 217, 178, 198, 218, 179, 199, 219, 180, 200, 220, 181,
         201, 221, 182, 202, 222, 183, 203, 223, 184, 204, 224, 185, 205, 225, 186,
         206, 226, 187, 207, 227, 188, 208, 228, 189, 209, 229, 190, 210, 230, 191,
         211, 231, 192, 212, 232, 193, 213, 233, 234, 260, 286, 235, 261, 287, 236,
         262, 288, 237, 263, 289, 238, 264, 290, 239, 265, 291, 240, 266, 292, 241,
         267, 293, 242, 268, 294, 243, 269, 295, 244, 270, 296, 245, 271, 297, 246,
         272, 298, 247, 273, 299, 248, 274, 300, 249, 275, 301, 250, 276, 302, 251,
         277, 303, 252, 278, 304, 253, 279, 305, 254, 280, 306, 255, 281, 307, 256,
         282, 308, 257, 283, 309, 258, 284, 310, 259, 285, 311, 312, 346, 380, 313,
         347, 381, 314, 348, 382, 315, 349, 383, 316, 350, 384, 317, 351, 385, 318,
         352, 386, 319, 353, 387, 320, 354, 388, 321, 355, 389, 322, 356, 390, 323,
         357, 391, 324, 358, 392, 325, 359, 393, 326, 360, 394, 327, 361, 395, 328,
         362, 396, 329, 363, 397, 330, 364, 398, 331, 365, 399, 332, 366, 400, 333,
         367, 401, 334, 368, 402, 335, 369, 403, 336, 370, 404, 337, 371, 405, 338,
         372, 406, 339, 373, 407, 340, 374, 408, 341, 375, 409, 342, 376, 410, 343,
         377, 411, 344, 378, 412, 345, 379, 413, 414, 456, 498, 415, 457, 499, 416,
         458, 500, 417, 459, 501, 418, 460, 502, 419, 461, 503, 420, 462, 504, 421,
         463, 505, 422, 464, 506, 423, 465, 507, 424, 466, 508, 425, 467, 509, 426,
         468, 510, 427, 469, 511, 428, 470, 512, 429, 471, 513, 430, 472, 514, 431,
         473, 515, 432, 474, 516, 433, 475, 517, 434, 476, 518, 435, 477, 519, 436,
         478, 520, 437, 479, 521, 438, 480, 522, 439, 481, 523, 440, 482, 524, 441,
         483, 525, 442, 484, 526, 443, 485, 527, 444, 486, 528, 445, 487, 529, 446,
         488, 530, 447, 489, 531, 448, 490, 532, 449, 491, 533, 450, 492, 534, 451,
         493, 535, 452, 494, 536, 453, 495, 537, 454, 496, 538, 455, 497, 539, 540,
         552, 564, 541, 553, 565, 542, 554, 566, 543, 555, 567, 544, 556, 568, 545,
         557, 569, 546, 558, 570, 547, 559, 571, 548, 560, 572, 549, 561, 573, 550,
         562, 574, 551, 563, 575]


