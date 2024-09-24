from gpaw.lcaotddft.restartfilewriter import RestartFileWriter
 
import numpy as np
from ase.units import Hartree, Bohr
from gpaw.lcaotddft import LCAOTDDFT
from litesoph.pre_processing.gpaw.dipolemomentwriter_mask import DipoleMomentWriter
from litesoph.pre_processing.gpaw.external_mask import MaskedElectricField
from gpaw.lcaotddft.laser import GaussianPulse
from gpaw.external import ConstantElectricField
pulse_0 = GaussianPulse(1e-05,8147.454712031825,4.26,2.355805781737317, 'sin')
mask_0 = {'Type': 'Plane', 'Boundary': 'Smooth', 'Axis': 2, 'X0': 0.5, 'Rsig': 0.1}
ext_0 = MaskedElectricField(Hartree / Bohr,[1, 0, 0], mask = mask_0 )
masks = [ext_0.mask,]
td_potential = [{'ext': ext_0, 'laser': pulse_0},]

td_calc = LCAOTDDFT(filename='../../gpaw/TaskTypes.GROUND_STATE/gs.gpw',
                    td_potential=td_potential,
                    txt='td.out')
RestartFileWriter(td_calc, 'td.gpw')
DipoleMomentWriter(td_calc, 'dm.dat', mask=None, interval=1)
DipoleMomentWriter(td_calc,'dm_masked_1.dat', mask=masks[0], interval=1)

# Propagate"
td_calc.propagate(10.0, 20000.0)
# Save the state for restarting later"
td_calc.write('td.gpw', mode='all')
