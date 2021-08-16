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
    Ind "Inuk.SG",
    Ind "JK2134",
    Ind "DAV-A-8_38.SG",
    Ind "I0041",
    Ind "523a.SG",
    Ind "HGDP01319.SDG",
    Ind "BDB001",
    Ind "GN01"]

-- August 13: Forging 50,000 SNPs takes about 9.5 seconds.
-- August 16: Forging 50,000 SNPs takes about 8.9 seconds... not much difference despite new algorithm... hmm
-- BUT found out that selectIndices is the main culprit, since in my test case I have selected all individuals per package. Have changed that now.

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
