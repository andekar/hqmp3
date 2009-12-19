module Codecs.Mp3.IMDCT (imdct) where

fi :: Int -> Double
fi = fromIntegral

-- Straightforward translation from the C code, elegant!
imdct18 :: [Double] -> [Double]
imdct18 xs = map (\s -> sum (zipWith (*) xs s)) lookupIMDCT
  where
    -- 36x18 matrix
    lookupIMDCT :: [[Double]]
    lookupIMDCT = [[ cos $ (pi / 18.0) * n * k
                     | k <- [0.5, 1.5 .. 17.5]] | n <- [9.5, 10.5 .. 44.5]]

-- Straightforward translation from the C code.
imdct :: Int -> [Double] -> [Double]
imdct 18 xs  = imdct18 xs
imdct pts xs = map (\n -> sum $ zipWith (subone n) xs [0..pts-1]) [0..2*pts-1]
  where
    subone :: Int -> Double -> Int -> Double
    subone n y k = y * (cos $ pipts * (fi n + 0.5 + nhalf) * (fi k + 0.5))
    pipts        = pi / (fi pts)
    nhalf        = (fi pts) / 2.0

{-
 - Until HybridFilterBank can handle arrays this code is useless, I'm afraid...
 -

-- Array version of the above, but is it any faster?
imdct18 :: UArray Int Double -> UArray Int Double
imdct18 xs = listArray (0,35) (map lookup [0..35])
  where
    -- This does the pairwise multiplication
    lookup :: Int -> Double
    lookup x = sum $ flip map [x*36 .. (x+1)*36-1] 
                   $ liftM2 (*) (lookupIMDCTArr !) (xs !)

-- Remains to be written
imdct :: Int -> UArray Int Double -> UArray Int Double
imdct 18 xs  = imdct18 xs
imdct pts xs = listArray (0,pts*2-1) $ map benny [0 .. 2*pts-1]
  where
    -- tribute to lucas jönefors
    benny :: Int -> Double
    benny n    = sum $ map (subone n) [0..pts-1]
    subone n k = let y = xs ! k
                 in y * (cos $ pipts * (fi n + 0.5 + nhalf) * (fi k + 0.5))
    nhalf = (fi pts) / 2.0
    pipts = pi / (fi pts)
-}

-- lookupIMDCT = listArray (0, 36*18-1) $ concat [[ cos $ (pi / 18.0) * n * k 
--   | k <- [0.5, 1.5 .. 17.5]] | n <- [9.5, 10.5 .. 44.5]]
-- 
--lookupIMDCTArr :: UArray Int Double
-- lookupIMDCTArr = array (0,647) [(0,0.6755902076156604),(1,-0.793353340291235),(2,-0.5372996083468242),(3,0.8870108331782215),(4,0.38268343236509045),(5,-0.9537169507482266),(6,-0.2164396139381039),(7,0.9914448613738103),(8,4.361938736533739e-2),(9,-0.9990482215818578),(10,0.13052619222004988),(11,0.9762960071199338),(12,-0.3007057995042712),(13,-0.9238795325112876),(14,0.46174861323503186),(15,0.843391445812887),(16,-0.6087614290087185),(17,-0.737277336810126),(18,0.6087614290087207),(19,-0.9238795325112867),(20,-0.13052619222005163),(21,0.9914448613738105),(22,-0.3826834323650899),(23,-0.7933533402912353),(24,0.7933533402912348),(25,0.38268343236509067),(26,-0.9914448613738103),(27,0.13052619222004988),(28,0.9238795325112875),(29,-0.6087614290087187),(30,-0.6087614290087229),(31,0.9238795325112855),(32,0.13052619222005513),(33,-0.9914448613738109),(34,0.3826834323650891),(35,0.7933533402912358),(36,0.5372996083468239),(37,-0.9914448613738104),(38,0.3007057995042726),(39,0.7372773368101242),(40,-0.9238795325112864),(41,4.361938736533463e-2),(42,0.8870108331782218),(43,-0.7933533402912347),(44,-0.21643961393810415),(45,0.9762960071199338),(46,-0.6087614290087187),(47,-0.46174861323503663),(48,0.9990482215818578),(49,-0.3826834323650892),(50,-0.6755902076156611),(51,0.9537169507482264),(52,-0.1305261922200493),(53,-0.8433914458128873),(54,0.4617486132350341),(55,-0.9914448613738105),(56,0.6755902076156595),(57,0.2164396139381038),(58,-0.9238795325112874),(59,0.8433914458128844),(60,-4.361938736533451e-2),(61,-0.7933533402912376),(62,0.9537169507482265),(63,-0.3007057995042712),(64,-0.6087614290087229),(65,0.9990482215818576),(66,-0.5372996083468201),(67,-0.3826834323650945),(68,0.9762960071199347),(69,-0.7372773368101219),(70,-0.130526192220059),(71,0.8870108331782238),(72,0.38268343236508984),(73,-0.9238795325112868),(74,0.9238795325112865),(75,-0.3826834323650899),(76,-0.38268343236509056),(77,0.9238795325112867),(78,-0.9238795325112864),(79,0.38268343236508956),(80,0.3826834323650909),(81,-0.9238795325112876),(82,0.9238795325112868),(83,-0.3826834323650892),(84,-0.3826834323650912),(85,0.9238795325112877),(86,-0.9238795325112854),(87,0.38268343236508556),(88,0.3826834323650883),(89,-0.9238795325112865),(90,0.3007057995042733),(91,-0.7933533402912354),(92,0.9990482215818578),(93,-0.8433914458128854),(94,0.3826834323650881),(95,0.21643961393810404),(96,-0.7372773368101256),(97,0.9914448613738108),(98,-0.8870108331782212),(99,0.46174861323503186),(100,0.13052619222005513),(101,-0.6755902076156637),(102,0.9762960071199339),(103,-0.9238795325112854),(104,0.5372996083468229),(105,4.361938736534191e-2),(106,-0.6087614290087234),(107,0.9537169507482294),(108,0.2164396139381029),(109,-0.6087614290087209),(110,0.8870108331782217),(111,-0.9990482215818577),(112,0.9238795325112864),(113,-0.6755902076156598),(114,0.3007057995042713),(115,0.13052619222005135),(116,-0.5372996083468248),(117,0.8433914458128852),(118,-0.9914448613738105),(119,0.9537169507482264),(120,-0.7372773368101219),(121,0.38268343236509217),(122,4.3619387365334814e-2),(123,-0.46174861323503413),(124,0.7933533402912362),(125,-0.976296007119934),(126,0.1305261922200517),(127,-0.38268343236509034),(128,0.608761429008721),(129,-0.7933533402912353),(130,0.9238795325112874),(131,-0.9914448613738106),(132,0.9914448613738103),(133,-0.9238795325112856),(134,0.7933533402912345),(135,-0.6087614290087185),(136,0.3826834323650858),(137,-0.1305261922200493),(138,-0.13052619222005196),(139,0.3826834323650949),(140,-0.6087614290087234),(141,0.7933533402912362),(142,-0.9238795325112893),(143,0.991444861373811),(144,4.361938736533601e-2),(145,-0.13052619222005163),(146,0.21643961393810293),(147,-0.30070579950427234),(148,0.38268343236509067),(149,-0.46174861323503325),(150,0.5372996083468247),(151,-0.6087614290087201),(152,0.675590207615661),(153,-0.7372773368101235),(154,0.7933533402912358),(155,-0.8433914458128873),(156,0.8870108331782206),(157,-0.9238795325112865),(158,0.9537169507482273),(159,-0.976296007119934),(160,0.9914448613738102),(161,-0.9990482215818577),(162,-4.361938736533589e-2),(163,0.13052619222005127),(164,-0.2164396139381032),(165,0.3007057995042715),(166,-0.38268343236508967),(167,0.461748613235032),(168,-0.5372996083468233),(169,0.6087614290087185),(170,-0.6755902076156595),(171,0.737277336810122),(172,-0.7933533402912321),(173,0.8433914458128838),(174,-0.8870108331782208),(175,0.9238795325112866),(176,-0.9537169507482252),(177,0.9762960071199325),(178,-0.99144486137381),(179,0.9990482215818577),(180,-0.1305261922200516),(181,0.38268343236509),(182,-0.6087614290087205),(183,0.7933533402912348),(184,-0.9238795325112871),(185,0.9914448613738103),(186,-0.9914448613738104),(187,0.9238795325112863),(188,-0.7933533402912357),(189,0.6087614290087203),(190,-0.38268343236509145),(191,0.13052619222004855),(192,0.13052619222005243),(193,-0.3826834323650885),(194,0.6087614290087234),(195,-0.7933533402912359),(196,0.9238795325112864),(197,-0.99144486137381),(198,-0.2164396139381028),(199,0.6087614290087205),(200,-0.8870108331782214),(201,0.9990482215818577),(202,-0.9238795325112875),(203,0.6755902076156608),(204,-0.3007057995042728),(205,-0.13052619222004952),(206,0.537299608346823),(207,-0.8433914458128858),(208,0.9914448613738106),(209,-0.9537169507482283),(210,0.7372773368101264),(211,-0.38268343236509195),(212,-4.361938736533482e-2),(213,0.4617486132350339),(214,-0.7933533402912315),(215,0.9762960071199324),(216,-0.30070579950427295),(217,0.7933533402912349),(218,-0.9990482215818577),(219,0.8433914458128868),(220,-0.3826834323650908),(221,-0.21643961393809924),(222,0.7372773368101222),(223,-0.9914448613738102),(224,0.8870108331782237),(225,-0.4617486132350402),(226,-0.13052619222004905),(227,0.6755902076156565),(228,-0.9762960071199317),(229,0.9238795325112881),(230,-0.5372996083468288),(231,-4.361938736532747e-2),(232,0.6087614290087174),(233,-0.9537169507482228),(234,-0.3826834323650897),(235,0.9238795325112865),(236,-0.9238795325112867),(237,0.38268343236509067),(238,0.38268343236508956),(239,-0.923879532511287),(240,0.9238795325112876),(241,-0.3826834323650912),(242,-0.382683432365089),(243,0.9238795325112867),(244,-0.9238795325112865),(245,0.3826834323650885),(246,0.3826834323650851),(247,-0.9238795325112851),(248,0.9238795325112881),(249,-0.3826834323650924),(250,-0.3826834323650813),(251,0.9238795325112835),(252,-0.46174861323503374),(253,0.9914448613738103),(254,-0.6755902076156606),(255,-0.21643961393810124),(256,0.9238795325112856),(257,-0.8433914458128869),(258,4.3619387365337874e-2),(259,0.7933533402912344),(260,-0.9537169507482283),(261,0.3007057995042734),(262,0.608761429008718),(263,-0.999048221581858),(264,0.5372996083468258),(265,0.3826834323650849),(266,-0.9762960071199316),(267,0.7372773368101269),(268,0.13052619222005146),(269,-0.8870108331782203),(270,-0.5372996083468236),(271,0.9914448613738106),(272,-0.30070579950427406),(273,-0.7372773368101224),(274,0.9238795325112875),(275,-4.3619387365337756e-2),(276,-0.8870108331782194),(277,0.7933533402912358),(278,0.2164396139380952),(279,-0.9762960071199317),(280,0.6087614290087263),(281,0.4617486132350278),(282,-0.999048221581858),(283,0.38268343236509555),(284,0.6755902076156559),(285,-0.9537169507482307),(286,0.13052619222005682),(287,0.8433914458128793),(288,-0.6087614290087207),(289,0.9238795325112867),(290,0.1305261922200519),(291,-0.9914448613738105),(292,0.3826834323650876),(293,0.7933533402912345),(294,-0.7933533402912336),(295,-0.382683432365089),(296,0.99144486137381),(297,-0.1305261922200522),(298,-0.923879532511288),(299,0.6087614290087209),(300,0.6087614290087232),(301,-0.9238795325112868),(302,-0.13052619222004808),(303,0.9914448613738104),(304,-0.3826834323650863),(305,-0.7933533402912397),(306,-0.6755902076156602),(307,0.7933533402912353),(308,0.5372996083468237),(309,-0.8870108331782218),(310,-0.38268343236508945),(311,0.953716950748227),(312,0.21643961393810235),(313,-0.9914448613738105),(314,-4.3619387365335306e-2),(315,0.9990482215818577),(316,-0.13052619222005243),(317,-0.9762960071199331),(318,0.3007057995042741),(319,0.9238795325112863),(320,-0.46174861323503497),(321,-0.843391445812885),(322,0.6087614290087217),(323,0.7372773368101231),(324,-0.737277336810124),(325,0.608761429008721),(326,0.8433914458128853),(327,-0.46174861323503325),(328,-0.9238795325112856),(329,0.30070579950427295),(330,0.9762960071199326),(331,-0.13052619222005196),(332,-0.9990482215818576),(333,-4.361938736533506e-2),(334,0.991444861373811),(335,0.2164396139381014),(336,-0.9537169507482285),(337,-0.3826834323650813),(338,0.8870108331782212),(339,0.5372996083468216),(340,-0.7933533402912392),(341,-0.6755902076156526),(342,-0.793353340291235),(343,0.38268343236509045),(344,0.9914448613738103),(345,0.13052619222004988),(346,-0.9238795325112876),(347,-0.6087614290087185),(348,0.608761429008726),(349,0.9238795325112867),(350,-0.13052619222005923),(351,-0.9914448613738106),(352,-0.38268343236508173),(353,0.7933533402912364),(354,0.7933533402912293),(355,-0.3826834323650926),(356,-0.991444861373809),(357,-0.13052619222004758),(358,0.9238795325112912),(359,0.6087614290087167),(360,-0.8433914458128855),(361,0.13052619222005263),(362,0.9537169507482275),(363,0.6755902076156571),(364,-0.382683432365091),(365,-0.999048221581858),(366,-0.4617486132350315),(367,0.6087614290087262),(368,0.9762960071199325),(369,0.21643961393809472),(370,-0.7933533402912385),(371,-0.8870108331782172),(372,4.361938736534999e-2),(373,0.9238795325112911),(374,0.7372773368101184),(375,-0.3007057995042783),(376,-0.9914448613738126),(377,-0.5372996083468121),(378,-0.8870108331782217),(379,-0.13052619222005202),(380,0.7372773368101243),(381,0.9762960071199335),(382,0.38268343236508934),(383,-0.537299608346825),(384,-0.9990482215818577),(385,-0.6087614290087209),(386,0.3007057995042736),(387,0.9537169507482274),(388,0.7933533402912337),(389,-4.361938736533922e-2),(390,-0.8433914458128879),(391,-0.9238795325112849),(392,-0.2164396139380971),(393,0.6755902076156652),(394,0.9914448613738113),(395,0.4617486132350389),(396,-0.9238795325112867),(397,-0.3826834323650899),(398,0.38268343236509067),(399,0.9238795325112875),(400,0.9238795325112868),(401,0.3826834323650891),(402,-0.38268343236509145),(403,-0.9238795325112865),(404,-0.9238795325112852),(405,-0.3826834323650883),(406,0.382683432365089),(407,0.9238795325112882),(408,0.9238795325112835),(409,0.3826834323650908),(410,-0.38268343236509306),(411,-0.9238795325112898),(412,-0.9238795325112873),(413,-0.38268343236508673),(414,-0.9537169507482268),(415,-0.6087614290087205),(416,-4.361938736533451e-2),(417,0.5372996083468247),(418,0.9238795325112876),(419,0.9762960071199326),(420,0.6755902076156566),(421,0.13052619222004538),(422,-0.46174861323503436),(423,-0.8870108331782225),(424,-0.99144486137381),(425,-0.7372773368101211),(426,-0.21643961393810426),(427,0.3826834323650962),(428,0.8433914458128864),(429,0.9990482215818574),(430,0.7933533402912328),(431,0.30070579950426146),(432,-0.9762960071199333),(433,-0.7933533402912348),(434,-0.46174861323503213),(435,-4.361938736533249e-2),(436,0.3826834323650911),(437,0.7372773368101261),(438,0.9537169507482272),(439,0.9914448613738096),(440,0.8433914458128836),(441,0.5372996083468162),(442,0.13052619222004466),(443,-0.30070579950427784),(444,-0.6755902076156624),(445,-0.9238795325112925),(446,-0.9990482215818572),(447,-0.8870108331782166),(448,-0.6087614290087136),(449,-0.21643961393809613),(450,-0.9914448613738104),(451,-0.9238795325112872),(452,-0.7933533402912347),(453,-0.6087614290087215),(454,-0.3826834323650892),(455,-0.1305261922200493),(456,0.13052619222004855),(457,0.3826834323650885),(458,0.6087614290087209),(459,0.7933533402912364),(460,0.9238795325112855),(461,0.9914448613738112),(462,0.9914448613738113),(463,0.9238795325112887),(464,0.7933533402912373),(465,0.6087614290087221),(466,0.3826834323650899),(467,0.13052619222005002),(468,-0.9990482215818578),(469,-0.9914448613738103),(470,-0.9762960071199335),(471,-0.9537169507482265),(472,-0.9238795325112868),(473,-0.8870108331782209),(474,-0.8433914458128857),(475,-0.7933533402912362),(476,-0.7372773368101214),(477,-0.6755902076156586),(478,-0.6087614290087201),(479,-0.5372996083468187),(480,-0.4617486132350363),(481,-0.3826834323650872),(482,-0.30070579950426524),(483,-0.21643961393810332),(484,-0.1305261922200466),(485,-4.361938736533971e-2),(486,-0.9990482215818578),(487,-0.9914448613738104),(488,-0.9762960071199337),(489,-0.953716950748227),(490,-0.9238795325112877),(491,-0.8870108331782222),(492,-0.8433914458128874),(493,-0.7933533402912384),(494,-0.7372773368101242),(495,-0.675590207615662),(496,-0.6087614290087185),(497,-0.5372996083468294),(498,-0.4617486132350354),(499,-0.38268343236509983),(500,-0.3007057995042788),(501,-0.2164396139381038),(502,-0.13052619222006168),(503,-4.3619387365341176e-2),(504,-0.9914448613738105),(505,-0.9238795325112874),(506,-0.7933533402912355),(507,-0.6087614290087229),(508,-0.3826834323650912),(509,-0.13052619222005196),(510,0.13052619222004538),(511,0.3826834323650851),(512,0.6087614290087177),(513,0.7933533402912336),(514,0.9238795325112835),(515,0.9914448613738095),(516,0.9914448613738112),(517,0.9238795325112886),(518,0.7933533402912373),(519,0.6087614290087224),(520,0.38268343236510394),(521,0.13052619222006556),(522,-0.9762960071199335),(523,-0.7933533402912353),(524,-0.4617486132350364),(525,-4.3619387365337756e-2),(526,0.3826834323650858),(527,0.7372773368101194),(528,0.9537169507482263),(529,0.991444861373811),(530,0.8433914458128896),(531,0.5372996083468321),(532,0.13052619222005682),(533,-0.3007057995042657),(534,-0.6755902076156526),(535,-0.9238795325112819),(536,-0.9990482215818585),(537,-0.8870108331782299),(538,-0.6087614290087255),(539,-0.21643961393811145),(540,-0.9537169507482269),(541,-0.6087614290087211),(542,-4.3619387365337506e-2),(543,0.5372996083468232),(544,0.9238795325112868),(545,0.9762960071199331),(546,0.6755902076156589),(547,0.13052619222004905),(548,-0.46174861323503064),(549,-0.8870108331782236),(550,-0.9914448613738107),(551,-0.7372773368101201),(552,-0.21643961393810332),(553,0.38268343236508356),(554,0.8433914458128864),(555,0.999048221581858),(556,0.7933533402912335),(557,0.30070579950427645),(558,-0.9238795325112868),(559,-0.38268343236509056),(560,0.38268343236508956),(561,0.9238795325112868),(562,0.9238795325112877),(563,0.3826834323650883),(564,-0.3826834323650885),(565,-0.9238795325112851),(566,-0.9238795325112868),(567,-0.3826834323650926),(568,0.3826834323650908),(569,0.9238795325112833),(570,0.9238795325112886),(571,0.38268343236509034),(572,-0.3826834323650799),(573,-0.9238795325112843),(574,-0.9238795325112876),(575,-0.38268343236508806),(576,-0.8870108331782218),(577,-0.13052619222005274),(578,0.7372773368101223),(579,0.9762960071199338),(580,0.3826834323650946),(581,-0.5372996083468198),(582,-0.9990482215818576),(583,-0.6087614290087239),(584,0.3007057995042697),(585,0.9537169507482238),(586,0.7933533402912412),(587,-4.3619387365326494e-2),(588,-0.8433914458128808),(589,-0.9238795325112901),(590,-0.21643961393811098),(591,0.6755902076156545),(592,0.9914448613738114),(593,0.4617486132350402),(594,-0.8433914458128858),(595,0.13052619222005013),(596,0.9537169507482265),(597,0.675590207615661),(598,-0.382683432365089),(599,-0.9990482215818576),(600,-0.46174861323504063),(601,0.6087614290087177),(602,0.9762960071199349),(603,0.21643961393810618),(604,-0.7933533402912311),(605,-0.8870108331782264),(606,4.361938736532258e-2),(607,0.9238795325112857),(608,0.7372773368101281),(609,-0.3007057995042643),(610,-0.9914448613738087),(611,-0.5372996083468253),(612,-0.7933533402912352),(613,0.3826834323650898),(614,0.9914448613738104),(615,0.1305261922200516),(616,-0.9238795325112854),(617,-0.6087614290087178),(618,0.6087614290087178),(619,0.9238795325112854),(620,-0.13052619222004808),(621,-0.9914448613738108),(622,-0.38268343236509306),(623,0.7933533402912373),(624,0.7933533402912373),(625,-0.38268343236509306),(626,-0.9914448613738108),(627,-0.13052619222004808),(628,0.9238795325112854),(629,0.6087614290087178),(630,-0.7372773368101241),(631,0.6087614290087204),(632,0.843391445812885),(633,-0.46174861323503175),(634,-0.9238795325112864),(635,0.30070579950427034),(636,0.9762960071199333),(637,-0.13052619222004833),(638,-0.999048221581858),(639,-4.361938736533971e-2),(640,0.9914448613738104),(641,0.21643961393809996),(642,-0.9537169507482246),(643,-0.38268343236509395),(644,0.8870108331782212),(645,0.537299608346822),(646,-0.7933533402912298),(647,-0.6755902076156537)]
