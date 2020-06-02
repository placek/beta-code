module BetaCodeSpec (spec) where

import Test.Hspec
import BetaCode
import qualified Data.Text          as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString    as BS

unicodeExample01 = "Ἐν ἀρχῇ ἦν ὁ λόγος, καὶ ὁ λόγος ἦν πρὸς τὸν θεόν, καὶ θεὸς ἦν ὁ λόγος."
unicodeExample02 = "οὗτος ἦν ἐν ἀρχῇ πρὸς τὸν θεόν."
unicodeExample03 = "πάντα δι᾽ αὐτοῦ ἐγένετο, καὶ χωρὶς αὐτοῦ ἐγένετο οὐδὲ ἕν. ὃ γέγονεν"
unicodeExample04 = "ἐν αὐτῷ ζωὴ ἦν, καὶ ἡ ζωὴ ἦν τὸ φῶς τῶν ἀνθρῴπων·"
unicodeExample05 = "καὶ τὸ φῶς ἐν τῇ σκοτίᾳ φαίνει, καὶ ἡ σκοτία αὐτὸ οὐ κατέλαβεν."
unicodeExample06 = "Ἐγένετο ἄνθρωπος, ἀπεσταλμένος παρὰ θεοῦ, ὄνομα αὐτῷ Ἰωάννης·"
unicodeExample07 = "οὗτος ἦλθεν εἰς μαρτυρίαν ἵνα μαρτυρήσῃ περὶ τοῦ φωτός, ἵνα πάντες πιστεύσωσιν δι᾽ αὐτοῦ."
unicodeExample08 = "οὐκ ἦν ἐκεῖνος τὸ φῶς, ἀλλ᾽ ἵνα μαρτυρήσῃ περὶ τοῦ φωτός."
unicodeExample09 = "Ἦν τὸ φῶς τὸ ἀληθινόν, ὃ φωτίζει πάντα ἄνθρωπον, ἐρχόμενον εἰς τὸν κόσμον."
unicodeExample10 = "ἐν τῷ κόσμῳ ἦν, καὶ ὁ κόσμος δι᾽ αὐτοῦ ἐγένετο, καὶ ὁ κόσμος αὐτὸν οὐκ ἔγνω."
unicodeExample11 = "εἰς τὰ ἴδια ἦλθεν, καὶ οἱ ἴδιοι αὐτὸν οὐ παρέλαβον."
unicodeExample12 = "ὅσοι δὲ ἔλαβον αὐτόν, ἔδωκεν αὐτοῖς ἐξουσίαν τέκνα θεοῦ γενέσθαι, τοῖς πιστεύουσιν εἰς τὸ ὄνομα αὐτοῦ,"
unicodeExample13 = "οἳ οὐκ ἐξ αἱμάτων οὐδὲ ἐκ θελήματος σαρκὸς οὐδὲ ἐκ θελήματος ἀνδρὸς ἀλλ᾽ ἐκ θεοῦ ἐγεννήθησαν."
unicodeExample14 = "Καὶ ὁ λόγος σὰρξ ἐγένετο καὶ ἐσκήνωσεν ἐν ἡμῖν, καὶ ἐθεασάμεθα τὴν δόξαν αὐτοῦ, δόξαν ὡς μονογενοῦς παρὰ πατρός, πλήρης χάριτος καὶ ἀληθείας."
unicodeExample15 = "Ἰωάννης μαρτυρεῖ περὶ αὐτοῦ καὶ κέκραγεν λέγων, Οὗτος ἦν ὃν εἶπον, Ὁ ὀπίσω μου ἐρχόμενος ἔμπροσθέν μου γέγονεν, ὅτι πρῶτός μου ἦν."
unicodeExample16 = "ὅτι ἐκ τοῦ πληρώματος αὐτοῦ ἡμεῖς πάντες ἐλάβομεν καὶ χάριν ἀντὶ χάριτος·"
unicodeExample17 = "ὅτι ὁ νόμος διὰ Μωϋσέως ἐδόθη, ἡ χάρις καὶ ἡ ἀλήθεια διὰ Ἰησοῦ Χριστοῦ ἐγένετο."
unicodeExample18 = "θεὸν οὐδεὶς ἑώρακεν πώποτε· μονογενὴς θεὸς ὁ ὢν εἰς τὸν κόλπον τοῦ πατρὸς ἐκεῖνος ἐξηγήσατο."

betaCodeExample01 = "*)en a)rxh=| h)=n o( lo/goj), kai\\ o( lo/goj h)=n pro\\j to\\n qeo/n), kai\\ qeo\\j h)=n o( lo/goj."
betaCodeExample02 = "ou(=toj h)=n e)n a)rxh=| pro\\j to\\n qeo/n."
betaCodeExample03 = "pa/nta di' au)tou= e)ge/neto), kai\\ xwri\\j au)tou= e)ge/neto ou)de\\ e(/n. o(\\ ge/gonen"
betaCodeExample04 = "e)n au)tw=| zwh\\ h)=n), kai\\ h( zwh\\ h)=n to\\ fw=j tw=n a)nqrw/|pwn:"
betaCodeExample05 = "kai\\ to\\ fw=j e)n th=| skoti/a| fai/nei), kai\\ h( skoti/a au)to\\ ou) kate/laben."
betaCodeExample06 = "*)ege/neto a)/nqrwpoj), a)pestalme/noj para\\ qeou=), o)/noma au)tw=| *)iwa/nnhj:"
betaCodeExample07 = "ou(=toj h)=lqen ei)j marturi/an i(/na marturh/sh| peri\\ tou= fwto/j), i(/na pa/ntej pisteu/swsin di' au)tou=."
betaCodeExample08 = "ou)k h)=n e)kei=noj to\\ fw=j), a)ll' i(/na marturh/sh| peri\\ tou= fwto/j."
betaCodeExample09 = "*)=hn to\\ fw=j to\\ a)lhqino/n), o(\\ fwti/zei pa/nta a)/nqrwpon), e)rxo/menon ei)j to\\n ko/smon."
betaCodeExample10 = "e)n tw=| ko/smw| h)=n), kai\\ o( ko/smoj di' au)tou= e)ge/neto), kai\\ o( ko/smoj au)to\\n ou)k e)/gnw."
betaCodeExample11 = "ei)j ta\\ i)/dia h)=lqen), kai\\ oi( i)/dioi au)to\\n ou) pare/labon."
betaCodeExample12 = "o(/soi de\\ e)/labon au)to/n), e)/dwken au)toi=j e)cousi/an te/kna qeou= gene/sqai), toi=j pisteu/ousin ei)j to\\ o)/noma au)tou=),"
betaCodeExample13 = "oi(\\ ou)k e)c ai(ma/twn ou)de\\ e)k qelh/matoj sarko\\j ou)de\\ e)k qelh/matoj a)ndro\\j a)ll' e)k qeou= e)gennh/qhsan."
betaCodeExample14 = "*kai\\ o( lo/goj sa\\rc e)ge/neto kai\\ e)skh/nwsen e)n h(mi=n), kai\\ e)qeasa/meqa th\\n do/can au)tou=), do/can w(j monogenou=j para\\ patro/j), plh/rhj xa/ritoj kai\\ a)lhqei/aj."
betaCodeExample15 = "*)iwa/nnhj marturei= peri\\ au)tou= kai\\ ke/kragen le/gwn), *ou(=toj h)=n o(\\n ei)=pon), *(o o)pi/sw mou e)rxo/menoj e)/mprosqe/n mou ge/gonen), o(/ti prw=to/j mou h)=n."
betaCodeExample16 = "o(/ti e)k tou= plhrw/matoj au)tou= h(mei=j pa/ntej e)la/bomen kai\\ xa/rin a)nti\\ xa/ritoj:"
betaCodeExample17 = "o(/ti o( no/moj dia\\ *mwu+se/wj e)do/qh), h( xa/rij kai\\ h( a)lh/qeia dia\\ *)ihsou= *xristou= e)ge/neto."
betaCodeExample18 = "qeo\\n ou)dei\\j e(w/raken pw/pote: monogenh\\j qeo\\j o( w)\\n ei)j to\\n ko/lpon tou= patro\\j e)kei=noj e)chgh/sato."

encode :: String -> BS.ByteString
encode = TE.encodeUtf8 . T.pack

spec :: Spec
spec = do
  describe "toBetaCode" $ do
    it "converts" $ do
      (toBetaCode . encode $ unicodeExample01) `shouldBe` Just (encode betaCodeExample01)
      (toBetaCode . encode $ unicodeExample02) `shouldBe` Just (encode betaCodeExample02)
      (toBetaCode . encode $ unicodeExample03) `shouldBe` Just (encode betaCodeExample03)
      (toBetaCode . encode $ unicodeExample04) `shouldBe` Just (encode betaCodeExample04)
      (toBetaCode . encode $ unicodeExample05) `shouldBe` Just (encode betaCodeExample05)
      (toBetaCode . encode $ unicodeExample06) `shouldBe` Just (encode betaCodeExample06)
      (toBetaCode . encode $ unicodeExample07) `shouldBe` Just (encode betaCodeExample07)
      (toBetaCode . encode $ unicodeExample08) `shouldBe` Just (encode betaCodeExample08)
      (toBetaCode . encode $ unicodeExample09) `shouldBe` Just (encode betaCodeExample09)
      (toBetaCode . encode $ unicodeExample10) `shouldBe` Just (encode betaCodeExample10)
      (toBetaCode . encode $ unicodeExample11) `shouldBe` Just (encode betaCodeExample11)
      (toBetaCode . encode $ unicodeExample12) `shouldBe` Just (encode betaCodeExample12)
      (toBetaCode . encode $ unicodeExample13) `shouldBe` Just (encode betaCodeExample13)
      (toBetaCode . encode $ unicodeExample14) `shouldBe` Just (encode betaCodeExample14)
      (toBetaCode . encode $ unicodeExample15) `shouldBe` Just (encode betaCodeExample15)
      (toBetaCode . encode $ unicodeExample16) `shouldBe` Just (encode betaCodeExample16)
      (toBetaCode . encode $ unicodeExample17) `shouldBe` Just (encode betaCodeExample17)
      (toBetaCode . encode $ unicodeExample18) `shouldBe` Just (encode betaCodeExample18)

--  describe "fromBetaCode" $ do
--    it "converts" $ do
--      (fromBetaCode . encode $ betaCodeExample01) `shouldBe` Just (encode unicodeExample01)
--      (fromBetaCode . encode $ betaCodeExample02) `shouldBe` Just (encode unicodeExample02)
--      (fromBetaCode . encode $ betaCodeExample03) `shouldBe` Just (encode unicodeExample03)
--      (fromBetaCode . encode $ betaCodeExample04) `shouldBe` Just (encode unicodeExample04)
--      (fromBetaCode . encode $ betaCodeExample05) `shouldBe` Just (encode unicodeExample05)
--      (fromBetaCode . encode $ betaCodeExample06) `shouldBe` Just (encode unicodeExample06)
--      (fromBetaCode . encode $ betaCodeExample07) `shouldBe` Just (encode unicodeExample07)
--      (fromBetaCode . encode $ betaCodeExample08) `shouldBe` Just (encode unicodeExample08)
--      (fromBetaCode . encode $ betaCodeExample09) `shouldBe` Just (encode unicodeExample09)
--      (fromBetaCode . encode $ betaCodeExample10) `shouldBe` Just (encode unicodeExample10)
--      (fromBetaCode . encode $ betaCodeExample11) `shouldBe` Just (encode unicodeExample11)
--      (fromBetaCode . encode $ betaCodeExample12) `shouldBe` Just (encode unicodeExample12)
--      (fromBetaCode . encode $ betaCodeExample13) `shouldBe` Just (encode unicodeExample13)
--      (fromBetaCode . encode $ betaCodeExample14) `shouldBe` Just (encode unicodeExample14)
--      (fromBetaCode . encode $ betaCodeExample15) `shouldBe` Just (encode unicodeExample15)
--      (fromBetaCode . encode $ betaCodeExample16) `shouldBe` Just (encode unicodeExample16)
--      (fromBetaCode . encode $ betaCodeExample17) `shouldBe` Just (encode unicodeExample17)
--      (fromBetaCode . encode $ betaCodeExample18) `shouldBe` Just (encode unicodeExample18)
