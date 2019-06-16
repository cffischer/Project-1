
!======================================================================
 Module Nucleus_m
!======================================================================
!  RRMS values for the periodic table
!I. Angeli and K.P. Marinova, Atomic Data and Nuclear Data Tables 99 (2013) 69–95,
! When values are not available the defaut formula is taken to be
!    rrms = 0.836*a**(1/3) + 0.570
!  shown to be a good approximation for Z < 91 by
!W.R. Johnson, G.Soff, Atomic Data and Nuclear Data Tables {\bf 33}, p.405 (1985)
!----------------------------------------------------------------------

    Implicit none
    !>number of atoms in the table ordered by atomic number
    Integer, parameter :: n_atoms = 120
    !>number of atomic masses associated with each atomic number
    Integer, parameter :: n_range = 35
    !>a data statement containing the minimum atomic mass associated with each of the n_atoms
    Integer  :: a_min(n_atoms)
    !>a data statement containing the maximum atomic mass associated with each of the n_atoms
    Integer  :: a_max(n_atoms)
    !>a data statement containing the rrms values for each atomic mass associated with each atomic number
    Real(8)  :: rr(n_atoms, n_range)

    Data a_min /  1,  3,  6,  7, 10, 12, 14, 16, 19, 17, 20, 24, 27, 28, 31, 32, &
         35, 32, 38, 39, 42, 44, 51, 50, 50, 54, 59, 58, 63, 64, 69, 70, 75, 74, &
         79, 72, 76, 77, 86, 87, 90, 90,  0, 96,103,102,101,102,104,108,121,116, &
        127,116,118,120,135,136,141,132,  0,138,137,145,147,146,151,150,153,152, &
        161,170,181,180,185,184,182,178,183,181,188,182,202,192,  0,202,207,208, &
          0,227,  0,233,  0,238,241,242,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
          0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0 /

    Data a_max /  3,  8, 11, 11, 11, 14, 15, 18, 19, 28, 31, 26, 27, 30, 31, 36, &
         37, 46, 47, 50, 46, 50, 51, 54, 56, 58, 59, 64, 65, 70, 71, 76, 75, 82, &
         81, 96, 98,100,102,102,103,108,  0,104,103,110,109,120,127,132,123,136, &
        127,146,146,148,139,148,141,150,  0,154,159,160,159,164,165,170,172,176, &
        179,182,181,186,187,192,193,198,199,206,208,214,213,218,  0,222,228,232, &
          0,232,  0,238,  0,244,243,248,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
          0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0 /

!  Note the range rr(1:n) where rr(i) == rrms(z, a_min + i-1)
    Data rr(  1, 1: 3) / 0.8783,2.1421,1.7591 /
    Data rr(  2, 1: 6) / 1.9661,1.6755,0.0000,2.0660,0.0000,1.9239 /
    Data rr(  3, 1: 6) / 2.5890,2.4440,2.3390,2.2450,0.0000,2.4820 /
    Data rr(  4, 1: 5) / 2.6460,0.0000,2.5190,2.3550,2.4630 /
    Data rr(  5, 1: 2) / 2.4277,2.4060 /
    Data rr(  6, 1: 3) / 2.4702,2.4614,2.5025 /
    Data rr(  7, 1: 2) / 2.5582,2.6058 /
    Data rr(  8, 1: 3) / 2.6991,2.6932,2.7726 /
    Data rr(  9, 1: 1) / 2.8976 /
    Data rr( 10, 1:12) / 3.0413,2.9714,3.0082,3.0055,2.9695,2.9525,2.9104,2.9007, &
                  2.9316,2.9251,0.0000,2.9642 /
    Data rr( 11, 1:12) / 2.9718,3.0136,2.9852,2.9936,2.9735,2.9769,2.9928,3.0136, &
                  3.0400,3.0922,3.1180,3.1704 /
    Data rr( 12, 1: 3) / 3.0570,3.0284,3.0337 /
    Data rr( 13, 1: 1) / 3.0610 /
    Data rr( 14, 1: 3) / 3.1224,3.1176,3.1336 /
    Data rr( 15, 1: 1) / 3.1889 /
    Data rr( 16, 1: 5) / 3.2611,0.0000,3.2847,0.0000,3.2985 /
    Data rr( 17, 1: 3) / 3.3654,0.0000,3.3840 /
    Data rr( 18, 1:15) / 3.3468,3.3438,3.3654,3.3636,3.3905,3.3908,3.4028,3.4093, &
                  3.4274,3.4251,3.4354,3.4414,3.4454,0.0000,3.4377 /
    Data rr( 19, 1:10) / 3.4264,3.4349,3.4381,3.4518,3.4517,3.4556,3.4563,3.4605, &
                  3.4558,3.4534 /
    Data rr( 20, 1:12) / 3.4595,3.4776,3.4780,3.5081,3.4954,3.5179,3.4944,3.4953, &
                  3.4783,3.4771,0.0000,3.5168 /
    Data rr( 21, 1: 5) / 3.5702,3.5575,3.5432,3.5459,3.5243 /
    Data rr( 22, 1: 7) / 3.6115,3.5939,3.6070,3.5962,3.5921,3.5733,3.5704 /
    Data rr( 23, 1: 1) / 3.6002 /
    Data rr( 24, 1: 5) / 3.6588,0.0000,3.6452,3.6511,3.6885 /
    Data rr( 25, 1: 7) / 3.7120,3.7026,3.6706,3.6662,3.6834,3.7057,3.7146 /
    Data rr( 26, 1: 5) / 3.6933,0.0000,3.7377,3.7532,3.7745 /
    Data rr( 27, 1: 1) / 3.7875 /
    Data rr( 28, 1: 7) / 3.7757,0.0000,3.8118,3.8225,3.8399,0.0000,3.8572 /
    Data rr( 29, 1: 3) / 3.8823,0.0000,3.9022 /
    Data rr( 30, 1: 7) / 3.9283,0.0000,3.9491,3.9530,3.9658,0.0000,3.9845 /
    Data rr( 31, 1: 3) / 3.9973,0.0000,4.0118 /
    Data rr( 32, 1: 7) / 4.0414,0.0000,4.0576,4.0632,4.0742,0.0000,4.0811 /
    Data rr( 33, 1: 1) / 4.0968 /
    Data rr( 34, 1: 9) / 4.0700,0.0000,4.1395,4.1395,4.1406,0.0000,4.1400,0.0000, &
                  4.1400 /
    Data rr( 35, 1: 3) / 4.1629,0.0000,4.1599 /
    Data rr( 36, 1:25) / 4.1635,0.0000,4.1870,4.2097,4.2020,4.2082,4.2038,4.2034, &
                  4.1970,4.1952,4.1919,4.1871,4.1884,4.1846,4.1835,4.1984,4.2171, &
                  4.2286,4.2423,4.2543,4.2724,4.2794,4.3002,4.3067,4.3267 /
    Data rr( 37, 1:23) / 4.2273,4.2356,4.2385,4.2284,4.2271,4.2213,4.2160,4.2058, &
                  4.1999,4.2036,4.2025,4.1989,4.2170,4.2391,4.2554,4.2723,4.2903, &
                  4.3048,4.3184,4.3391,4.3501,4.4231,4.4336 /
    Data rr( 38, 1:24) / 4.2569,4.2561,4.2586,4.2562,4.2547,4.2478,4.2455,4.2394, &
                  4.2304,4.2307,4.2249,4.2240,4.2407,4.2611,4.2740,4.2924,4.3026, &
                  4.3191,4.3305,4.3522,4.3625,4.4377,4.4495,4.4640 /
    Data rr( 39, 1:17) / 4.2513,4.2498,4.2441,4.2430,4.2573,0.0000,4.2887,4.3052, &
                  4.3142,4.3284,4.3402,4.3580,4.3711,4.4658,4.4705,4.4863,4.4911 /
    Data rr( 40, 1:16) / 4.2789,4.2787,4.2706,4.2694,4.2845,4.3057,0.0000,4.3320, &
                  0.0000,4.3512,4.3792,4.4012,4.4156,4.4891,4.5119,4.5292 /
    Data rr( 41, 1:14) / 4.2891,4.2878,4.3026,4.3240,0.0000,0.0000,0.0000,0.0000, &
                  0.0000,4.4062,0.0000,4.4861,0.0000,4.5097 /
    Data rr( 42, 1:19) / 4.3265,4.3182,4.3151,0.0000,4.3529,4.3628,4.3847,4.3880, &
                  4.4091,0.0000,4.4468,0.0000,4.4914,4.5145,4.5249,4.5389,4.5490, &
                  0.0000,4.5602 /
    Data rr( 44, 1: 9) / 4.3908,0.0000,4.4229,4.4338,4.4531,4.4606,4.4809,0.0000, &
                  4.5098 /
    Data rr( 45, 1: 1) / 4.4945 /
    Data rr( 46, 1: 9) / 4.4827,0.0000,4.5078,4.5150,4.5318,0.0000,4.5563,0.0000, &
                  4.5782 /
    Data rr( 47, 1: 9) / 4.4799,0.0000,4.5036,4.5119,4.5269,0.0000,4.5454,0.0000, &
                  4.5638 /
    Data rr( 48, 1:19) / 4.4810,4.4951,4.5122,4.5216,4.5383,4.5466,4.5577,4.5601, &
                  4.5765,4.5845,4.5944,4.6012,4.6087,4.6114,4.6203,4.6136,4.6246, &
                  0.0000,4.6300 /
    Data rr( 49, 1:24) / 4.5184,4.5311,4.5375,4.5494,4.5571,4.5685,4.5742,4.5856, &
                  4.5907,4.6010,4.6056,4.6156,4.6211,4.6292,4.6335,4.6407,4.6443, &
                  4.6505,4.6534,4.6594,4.6625,4.6670,4.6702,4.6733 /
    Data rr( 50, 1:25) / 4.5605,4.5679,4.5785,4.5836,4.5948,4.6015,4.6099,4.6148, &
                  4.6250,4.6302,4.6393,4.6438,4.6519,4.6566,4.6634,4.6665,4.6735, &
                  4.6765,4.6833,4.6867,4.6921,4.6934,4.7019,4.7078,4.7093 /
    Data rr( 51, 1: 3) / 4.6802,0.0000,4.6879 /
    Data rr( 52, 1:21) / 4.6847,0.0000,4.6956,0.0000,4.7038,0.0000,4.7095,4.7117, &
                  4.7183,4.7204,4.7266,0.0000,4.7346,0.0000,4.7423,0.0000,4.7500, &
                  0.0000,4.7569,0.0000,4.7815 /
    Data rr( 53, 1: 1) / 4.7500 /
    Data rr( 54, 1:31) / 4.7211,0.0000,4.7387,0.0000,4.7509,0.0000,4.7590,0.0000, &
                  4.7661,0.0000,4.7722,4.7747,4.7774,4.7775,4.7818,4.7808,4.7859, &
                  4.7831,4.7899,0.0000,4.7964,4.8094,4.8279,4.8409,4.8566,4.8694, &
                  4.8841,4.8942,4.9082,0.0000,4.9315 /
    Data rr( 55, 1:29) / 4.7832,4.7896,4.7915,4.7769,4.7773,4.7820,4.7828,4.7880, &
                  4.7872,4.7936,4.7921,4.7981,4.7992,4.8026,4.8002,4.8041,4.8031, &
                  4.8067,4.8059,4.8128,4.8255,4.8422,4.8554,4.8689,4.8825,4.8965, &
                  4.9055,4.9188,4.9281 /
    Data rr( 56, 1:29) / 4.8092,4.8176,4.8153,4.8135,4.8185,4.8177,4.8221,4.8204, &
                  4.8255,4.8248,4.8283,4.8276,4.8303,4.8286,4.8322,4.8294,4.8334, &
                  4.8314,4.8378,4.8513,4.8684,4.8807,4.8953,4.9087,4.9236,4.9345, &
                  4.9479,0.0000,4.9731 /
    Data rr( 57, 1: 5) / 4.8488,0.0000,4.8496,4.8473,4.8550 /
    Data rr( 58, 1:13) / 4.8739,0.0000,4.8737,0.0000,4.8771,0.0000,4.9063,0.0000, &
                  4.9303,0.0000,4.9590,0.0000,4.9893 /
    Data rr( 59, 1: 1) / 4.8919 /
    Data rr( 60, 1:19) / 4.9174,0.0000,4.9128,4.9086,4.9111,4.9080,4.9123,4.9076, &
                  4.9101,4.9057,4.9123,4.9254,4.9421,4.9535,4.9696,0.0000,4.9999, &
                  0.0000,5.0400 /
    Data rr( 62, 1:17) / 4.9599,4.9556,4.9565,4.9517,4.9518,4.9479,4.9524,4.9651, &
                  4.9808,4.9892,5.0042,5.0134,5.0387,5.0550,5.0819,5.0925,5.1053 /
    Data rr( 63, 1:23) / 4.9762,4.9779,4.9760,4.9695,4.9697,4.9607,4.9636,4.9612, &
                  4.9663,4.9789,4.9938,5.0045,5.0202,5.0296,5.0522,5.1064,5.1115, &
                  5.1239,5.1221,5.1264,5.1351,5.1413,5.1498 /
    Data rr( 64, 1:16) / 4.9786,4.9801,0.0000,5.0080,0.0000,5.0342,0.0000,5.0774, &
                  0.0000,5.1223,5.1319,5.1420,5.1449,5.1569,0.0000,5.1734 /
    Data rr( 65, 1:13) / 4.9201,4.9291,4.9427,4.9499,4.9630,4.9689,4.9950,5.0333, &
                  5.0391,0.0000,5.0489,0.0000,5.0600 /
    Data rr( 66, 1:19) / 5.0438,0.0000,5.0455,5.0567,5.0706,5.0801,5.0950,5.1035, &
                  5.1241,5.1457,5.1622,5.1709,5.1815,5.1825,5.1951,5.1962,5.2074, &
                  5.2099,5.2218 /
    Data rr( 67, 1:15) / 5.0398,5.0614,5.0760,5.0856,5.1076,5.1156,5.1535,5.1571, &
                  5.1675,5.1662,5.1785,5.1817,5.1907,0.0000,5.2022 /
    Data rr( 68, 1:21) / 5.0548,0.0000,5.0843,0.0000,5.1129,0.0000,5.1429,0.0000, &
                  5.1761,0.0000,5.2045,0.0000,5.2246,0.0000,5.2389,0.0000,5.2516, &
                  5.2560,5.2644,0.0000,5.2789 /
    Data rr( 69, 1:20) / 5.0643,5.0755,0.0000,5.0976,5.1140,5.1235,5.1392,5.1504, &
                  5.1616,5.1713,5.1849,5.1906,5.2004,5.2046,5.2129,5.2170,5.2256, &
                  5.2303,5.2388,5.2411 /
    Data rr( 70, 1:25) / 5.0423,0.0000,5.0875,5.1040,5.1219,5.1324,5.1498,5.1629, &
                  5.1781,5.1889,5.2054,5.2157,5.2307,5.2399,5.2525,5.2621,5.2702, &
                  5.2771,5.2853,5.2906,5.2995,5.3046,5.3108,5.3135,5.3215 /
    Data rr( 71, 1:19) / 5.2293,5.2398,5.2567,5.2677,5.2830,5.2972,5.3108,5.3227, &
                  5.3290,5.3364,5.3436,5.3486,5.3577,5.3634,5.3700,5.3739,5.3815, &
                  5.3857,5.3917 /
    Data rr( 72, 1:13) / 5.2898,5.3041,5.3065,5.3140,5.3201,5.3191,5.3286,5.3309, &
                  5.3371,5.3408,5.3470,0.0000,5.3516 /
    Data rr( 73, 1: 1) / 5.3507 /
    Data rr( 74, 1: 7) / 5.3491,0.0000,5.3559,5.3611,5.3658,0.0000,5.3743 /
    Data rr( 75, 1: 3) / 5.3596,0.0000,5.3698 /
    Data rr( 76, 1: 9) / 5.3823,0.0000,5.3909,5.3933,5.3993,5.4016,5.4062,0.0000, &
                  5.4126 /
    Data rr( 77, 1:12) / 5.3705,5.3780,5.3805,5.3854,5.3900,5.3812,5.3838,5.3898, &
                  0.0000,5.3968,0.0000,5.4032 /
    Data rr( 78, 1:21) / 5.3728,5.3915,5.3891,5.3996,5.3969,5.4038,5.4015,5.4148, &
                  5.4037,5.4063,5.4053,5.4060,5.4108,5.4102,5.4169,5.4191,5.4236, &
                  5.4270,5.4307,0.0000,5.4383 /
    Data rr( 79, 1:17) / 5.4247,5.4306,5.4296,5.4354,5.4018,5.4049,5.4084,5.4109, &
                  5.4147,5.4179,5.4221,5.4252,5.4298,5.4332,5.4371,5.4400,5.4454 /
    Data rr( 80, 1:26) / 5.4364,5.3833,5.4405,5.3949,5.4397,5.4017,5.4046,5.4085, &
                  5.4100,5.4158,5.4171,5.4232,5.4238,5.4309,5.4345,5.4385,5.4412, &
                  5.4463,5.4474,5.4551,5.4581,5.4648,5.4679,5.4744,5.4776,5.4837 /
    Data rr( 81, 1:21) / 5.4017,0.0000,5.4121,5.4169,5.4191,5.4243,5.4259,5.4325, &
                  5.4327,5.4388,5.4396,5.4479,5.4491,5.4573,5.4595,5.4666,5.4704, &
                  5.4759,0.0000,5.4853,5.4946 /
    Data rr( 82, 1:33) / 5.3788,5.3869,5.3930,5.3984,5.4027,5.4079,5.4139,5.4177, &
                  5.4222,5.4229,5.4300,5.4310,5.4372,5.4389,5.4444,5.4446,5.4524, &
                  5.4529,5.4611,5.4629,5.4705,5.4727,5.4803,5.4828,5.4902,5.4943, &
                  5.5012,5.5100,5.5208,5.5290,5.5396,0.0000,5.5577 /
    Data rr( 83, 1:12) / 5.4840,5.4911,5.4934,5.5008,5.5034,5.5103,5.5147,5.5211, &
                  5.5300,0.0000,5.5489,5.5586 /
    Data rr( 84, 1:27) / 5.5220,0.0000,5.5167,0.0000,5.5136,0.0000,5.5146,0.0000, &
                  5.5199,0.0000,5.5281,0.0000,5.5378,5.5389,5.5480,5.5501,5.5584, &
                  5.5628,5.5704,0.0000,0.0000,0.0000,0.0000,0.0000,5.6359,0.0000, &
                  5.6558 /
    Data rr( 86, 1:21) / 5.5521,0.0000,5.5568,5.5569,5.5640,5.5652,5.5725,5.5743, &
                  5.5813,5.5850,5.5915,0.0000,0.0000,0.0000,0.0000,0.0000,5.6540, &
                  5.6648,5.6731,5.6834,5.6915 /
    Data rr( 87, 1:22) / 5.5720,5.5729,5.5799,5.5818,5.5882,5.5915,5.5977,0.0000, &
                  0.0000,0.0000,0.0000,0.0000,0.0000,5.6688,5.6790,5.6890,5.6951, &
                  5.7061,5.7112,5.7190,5.7335,5.7399 /
    Data rr( 88, 1:25) / 5.5850,5.5853,5.5917,5.5929,5.5991,5.6020,5.6079,0.0000, &
                  0.0000,0.0000,0.0000,0.0000,5.6683,5.6795,5.6874,5.6973,5.7046, &
                  5.7150,5.7211,5.7283,5.7370,5.7455,5.7551,0.0000,5.7714 /
    Data rr( 90, 1: 6) / 5.7404,5.7488,5.7557,5.7670,0.0000,5.7848 /
    Data rr( 92, 1: 6) / 5.8203,5.8291,5.8337,5.8431,0.0000,5.8571 /
    Data rr( 94, 1: 7) / 5.8535,5.8601,5.8701,5.8748,5.8823,0.0000,5.8948 /
    Data rr( 95, 1: 3) / 5.8928,0.0000,5.9048 /
    Data rr( 96, 1: 7) / 5.8285,0.0000,5.8429,5.8475,5.8562,0.0000,5.8687 /

CONTAINS
!======================================================================
!> Function takes in as parameters: (iz,a) where 'iz' is the atomic number and 'a' is the atomic mass
!!
!> If the parameters iz,a are in the table rr,
!! then use the corresponding rrms value associated with the iz,a pair.
!! Otherwise the function will handle calculating an approximate value for the rrms value.
   Real(8) Function RRMS_value(iz,a)
!======================================================================
!  RRMS values for the given Z and A
!----------------------------------------------------------------------
     IMPLICIT NONE
     INTEGER  i,a, iz, n

     rrms_value  = 0.d0
     i = a - a_min(iz) +1
     n = a_max(iz) - a_min(iz) +1

     If ( i >= 1  .and. i <= n ) then
         rrms_value = rr(iz, i)
     End if
     If  ( rrms_value == 0.d0 ) then
         rrms_value = 0.836*a**(1/3.d0) + 0.570
     End if
   END FUNCTION RRMS_value

 END Module Nucleus_m


    PROGRAM  test_nucleus
     Use Nucleus_m

     IMPLICIT NONE
     INTEGER  a, iz

     print *, 'ENter the atomic number and mass nubmer'
     Read  *, iz, a
     If (iz >= n_atoms ) Print *, ' Atomic number out of range'

     Print *, ' Atomic number           = ', iz
     Print *, ' Nucleur Mass number     = ', a

     Print *, ' RRMS =', rrms_value(iz,a)

    End program test_nucleus




