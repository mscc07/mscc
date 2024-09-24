
from gpaw import GPAW
from gpaw.lcaotddft import LCAOTDDFT
from gpaw.lcaotddft.ksdecomposition import KohnShamDecomposition
from gpaw.lcaotddft.densitymatrix import DensityMatrix
from litesoph.post_processing.gpaw.mopopulationwriter import MoPopulationWriter

calc = GPAW('../../gpaw/TaskTypes.GROUND_STATE/gs.gpw', txt=None).fixed_density(nbands='nao', txt='unocc.out')
calc.write('unocc.gpw', mode='all')

td_calc = LCAOTDDFT('unocc.gpw', txt=None)

dmat = DensityMatrix(td_calc)
ksd = KohnShamDecomposition(td_calc)
ksd.initialize(td_calc)
MoPopulationWriter(td_calc, dmat, ksd, filename='mo_population.dat')
# Replay the propagation
td_calc.replay(name='../../gpaw/TaskTypes.RT_TDDFT/wf.ulm', update='none')

