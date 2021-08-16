import           System.Directory (getHomeDirectory, removePathForcibly)
import           System.FilePath  ((</>))

import Poseidon.CLI.Forge (ForgeOptions(..), runForge)
import Poseidon.EntitiesList      (PoseidonEntity(..))
import Poseidon.GenotypeData (GenotypeFormatSpec(..))

-- Dumb hard-coding: these just happen to be the ones that I have on my computer right now.
entities :: [PoseidonEntity]
entities = [
    Pac "2010_RasmussenNature",
    Pac "2017_Schuenemann_EgyptianMummies",
    Pac "2018_Ebenesersdottir_Iceland",
    Pac "2018_Posth_SouthAmerica",
    Pac "2018_ScheibScience",
    Pac "2020_Rivollat_FranceGermany",
    Pac "2020_Wang_EastAsia_modern",
    Pac "2020_Bergstrom_HGDP"]

entities2 :: [PoseidonEntity]
entities2 = [
    Group "Greenland_Saqqaq.SG",
    Group "Egypt_ThirdIntermediatePeriod",
    Group "Iceland_Pre_Christian.SG",
    Group "Peru_Lauricocha_8600BP",
    Group "Alaskan_Athabskan.SG",
    Group "Germany_Meso_BDB",
    Group "Tibetan_Gannan",
    Group "China_Lahu.SDG"]

-- August 13: Forging 50,000 SNPs takes about 9.5 seconds.
-- August 16: Forging 50,000 SNPs takes about 8.9 seconds... not much difference despite new algorithm... hmm

main :: IO ()
main = do
    h <- getHomeDirectory
    -- Dumb hard-coding: this just happens to be the place where it's on my computer.
    let baseDir = h </> "poseidon/repo"
    let forgeOptions = ForgeOptions {
        _forgeBaseDirs     = [baseDir],
        _forgeEntityList   = entities2,
        _forgeEntityFiles  = [],
        _forgeIntersect    = False,
        _forgeOutPacPath   = "/tmp/outPac",
        _forgeOutPacName   = "testForgePac",
        _forgeOutFormat    = GenotypeFormatPlink,
        _forgeShowWarnings = False
    }
    removePathForcibly "/tmp/outPac"
    runForge forgeOptions
