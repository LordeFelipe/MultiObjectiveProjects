#!/usr/bin/env python
"""
LCOE_csm_ssembly.py

Created by NWTC Systems Engineering Sub-Task on 2012-08-01.
Copyright (c) NREL. All rights reserved.
"""

import sys
import os
import numpy as np
import pandas as pd

from openmdao.main.api import Assembly, Component
from openmdao.main.datatypes.api import Int, Float, Enum, VarTree, Bool, Str, Array


from fusedwind.plant_cost.fused_finance import configure_base_financial_analysis, configure_extended_financial_analysis, ExtendedFinancialAnalysis
from fusedwind.plant_cost.fused_opex import OPEXVarTree
from fusedwind.plant_cost.fused_bos_costs import BOSVarTree
from fusedwind.interface import implement_base

from wisdem.turbinese.turbine import configure_turbine
from turbine_costsse.turbine_costsse import Turbine_CostsSE
from plant_costsse.nrel_csm_bos.nrel_csm_bos import bos_csm_assembly
from plant_costsse.nrel_csm_opex.nrel_csm_opex import opex_csm_assembly
from plant_costsse.nrel_land_bosse.nrel_land_bosse import NREL_Land_BOSSE
from plant_costsse.ecn_offshore_opex.ecn_offshore_opex  import opex_ecn_assembly
from plant_financese.nrel_csm_fin.nrel_csm_fin import fin_csm_assembly
from fusedwind.plant_flow.basic_aep import aep_assembly, aep_weibull_assembly

# Current configuration assembly options for LCOE SE
# Turbine Costs
def configure_lcoe_with_turb_costs(assembly):
    """
    tcc_a inputs:
        advanced_blade = Bool
        offshore = Bool
        assemblyCostMultiplier = Float
        overheadCostMultiplier = Float
        profitMultiplier = Float
        transportMultiplier = Float
    """

    #assembly.replace('tcc_a', Turbine_CostsSE())

    assembly.add('advanced_blade', Bool(True, iotype='in', desc='advanced (True) or traditional (False) blade design'))
    assembly.add('offshore', Bool(iotype='in', desc='flag for offshore site'))
    assembly.add('assemblyCostMultiplier',Float(0.0, iotype='in', desc='multiplier for assembly cost in manufacturing'))
    assembly.add('overheadCostMultiplier', Float(0.0, iotype='in', desc='multiplier for overhead'))
    assembly.add('profitMultiplier', Float(0.0, iotype='in', desc='multiplier for profit markup'))
    assembly.add('transportMultiplier', Float(0.0, iotype='in', desc='multiplier for transport costs'))

    # connections to turbine costs
    assembly.connect('rotor.mass_one_blade', 'tcc_a.blade_mass')
    assembly.connect('hub.hub_mass', 'tcc_a.hub_mass')
    assembly.connect('hub.pitch_system_mass', 'tcc_a.pitch_system_mass')
    assembly.connect('hub.spinner_mass', 'tcc_a.spinner_mass')
    assembly.connect('nacelle.low_speed_shaft_mass', 'tcc_a.low_speed_shaft_mass')
    assembly.connect('nacelle.main_bearing_mass', 'tcc_a.main_bearing_mass')
    assembly.connect('nacelle.second_bearing_mass', 'tcc_a.second_bearing_mass')
    assembly.connect('nacelle.gearbox_mass', 'tcc_a.gearbox_mass')
    assembly.connect('nacelle.high_speed_side_mass', 'tcc_a.high_speed_side_mass')
    assembly.connect('nacelle.generator_mass', 'tcc_a.generator_mass')
    assembly.connect('nacelle.bedplate_mass', 'tcc_a.bedplate_mass')
    assembly.connect('nacelle.yaw_system_mass', 'tcc_a.yaw_system_mass')
    assembly.connect('tower.mass', 'tcc_a.tower_mass')
    assembly.connect('rotor.control.ratedPower', 'tcc_a.machine_rating')
    assembly.connect('rotor.nBlades', 'tcc_a.blade_number')
    assembly.connect('nacelle.crane', 'tcc_a.crane')
    assembly.connect('year', 'tcc_a.year')
    assembly.connect('month', 'tcc_a.month')
    assembly.connect('nacelle.drivetrain_design', 'tcc_a.drivetrain_design')
    assembly.connect('advanced_blade','tcc_a.advanced_blade')
    assembly.connect('offshore','tcc_a.offshore')
    assembly.connect('assemblyCostMultiplier','tcc_a.assemblyCostMultiplier')
    assembly.connect('overheadCostMultiplier','tcc_a.overheadCostMultiplier')
    assembly.connect('profitMultiplier','tcc_a.profitMultiplier')
    assembly.connect('transportMultiplier','tcc_a.transportMultiplier')

# Balance of Station Costs
def configure_lcoe_with_csm_bos(assembly):
    """
    bos inputs:
        bos_multiplier = Float
    """

    #assembly.replace('bos_a', bos_csm_assembly())

    assembly.add('bos_multiplier', Float(1.0, iotype='in'))

    # connections to bos
    assembly.connect('machine_rating', 'bos_a.machine_rating')
    assembly.connect('rotor.diameter', 'bos_a.rotor_diameter')
    assembly.connect('rotor.hubHt', 'bos_a.hub_height')
    assembly.connect('turbine_number', 'bos_a.turbine_number')
    assembly.connect('rotor.mass_all_blades + hub.hub_system_mass + nacelle.nacelle_mass', 'bos_a.RNA_mass')

    assembly.connect('sea_depth', 'bos_a.sea_depth')
    assembly.connect('year', 'bos_a.year')
    assembly.connect('month', 'bos_a.month')
    assembly.connect('bos_multiplier','bos_a.multiplier')

def configure_lcoe_with_landbos(assembly):
    """
    if with_landbos additional inputs:
        voltage
        distInter
        terrain
        layout
        soil
    """

    #assembly.replace('bos_a', NREL_Land_BOSSE())

    assembly.add('voltage', Float(iotype='in', units='kV', desc='interconnect voltage'))
    assembly.add('distInter', Float(iotype='in', units='mi', desc='distance to interconnect'))
    assembly.add('terrain', Enum('FLAT_TO_ROLLING', ('FLAT_TO_ROLLING', 'RIDGE_TOP', 'MOUNTAINOUS'),
        iotype='in', desc='terrain options'))
    assembly.add('layout', Enum('SIMPLE', ('SIMPLE', 'COMPLEX'), iotype='in',
        desc='layout options'))
    assembly.add('soil', Enum('STANDARD', ('STANDARD', 'BOUYANT'), iotype='in',
        desc='soil options'))
    assembly.add('transportDist',Float(0.0, iotype='in', units='mi', desc='transportation distance'))
    # TODO: add rest of land-bos connections

    # connections to bos
    assembly.connect('machine_rating', 'bos_a.machine_rating')
    assembly.connect('rotor.diameter', 'bos_a.rotor_diameter')
    assembly.connect('rotor.hubHt', 'bos_a.hub_height')
    assembly.connect('turbine_number', 'bos_a.turbine_number')
    assembly.connect('rotor.mass_all_blades + hub.hub_system_mass + nacelle.nacelle_mass', 'bos_a.RNA_mass')

    assembly.connect('voltage', 'bos_a.voltage')
    assembly.connect('distInter', 'bos_a.distInter')
    assembly.connect('terrain', 'bos_a.terrain')
    assembly.connect('layout', 'bos_a.layout')
    assembly.connect('soil', 'bos_a.soil')
    assembly.connect('transportDist','bos_a.transportDist')

# Operational Expenditures
def configure_lcoe_with_csm_opex(assembly):
    """
    opex inputs:
       availability = Float()
    """

    #assembly.replace('opex_a', opex_csm_assembly())

    # connections to opex
    assembly.connect('machine_rating', 'opex_a.machine_rating')
    assembly.connect('sea_depth', 'opex_a.sea_depth')
    assembly.connect('year', 'opex_a.year')
    assembly.connect('month', 'opex_a.month')
    assembly.connect('turbine_number', 'opex_a.turbine_number')
    assembly.connect('aep_a.net_aep', 'opex_a.net_aep')


def configure_lcoe_with_ecn_opex(assembly,ecn_file):

    #assembly.replace('opex_a', opex_ecn_assembly(ecn_file))

    assembly.connect('machine_rating', 'opex_a.machine_rating')
    assembly.connect('turbine_number', 'opex_a.turbine_number')
    assembly.connect('tcc_a.turbine_cost','opex_a.turbine_cost')
    assembly.connect('project_lifetime','opex_a.project_lifetime')

# Energy Production
def configure_lcoe_with_basic_aep(assembly):
    """
    aep inputs:
        array_losses = Float
        other_losses = Float
        availability = Float
    """

    #assembly.replace('aep_a', aep_assembly())

    assembly.add('array_losses',Float(0.059, iotype='in', desc='energy losses due to turbine interactions - across entire plant'))
    assembly.add('other_losses',Float(0.0, iotype='in', desc='energy losses due to blade soiling, electrical, etc'))

    # connections to aep
    assembly.connect('rotor.AEP', 'aep_a.AEP_one_turbine')
    assembly.connect('turbine_number', 'aep_a.turbine_number')
    assembly.connect('machine_rating','aep_a.machine_rating')
    assembly.connect('array_losses','aep_a.array_losses')
    assembly.connect('other_losses','aep_a.other_losses')

def configure_lcoe_with_weibull_aep(assembly):
    """
    aep inputs
        power_curve    = Array([], iotype='in', desc='wind turbine power curve')
        array_losses = Float
        other_losses = Float
        A = Float
        k = Float
    """

    assembly.add('array_losses',Float(0.059, iotype='in', desc='energy losses due to turbine interactions - across entire plant'))
    assembly.add('other_losses',Float(0.0, iotype='in', desc='energy losses due to blade soiling, electrical, etc'))
    assembly.add('A',Float(8.2,iotype='in', desc='scale factor'))
    assembly.add('k', Float(2.0,iotype='in', desc='shape or form factor'))

    #assembly.replace('aep_a', aep_weibull_assembly())
    
    assembly.connect('turbine_number', 'aep_a.turbine_number')
    assembly.connect('machine_rating','aep_a.machine_rating')
    assembly.connect('array_losses','aep_a.array_losses')
    assembly.connect('other_losses','aep_a.other_losses')
    assembly.connect('A','aep_a.A')
    assembly.connect('k','aep_a.k')
    assembly.connect('rotor.V','aep_a.wind_curve')
    assembly.connect('rotor.P','aep_a.power_curve')


# Finance
def configure_lcoe_with_csm_fin(assembly):
    """
    fin inputs:
        fixed_charge_rate = Float
        construction_finance_rate = Float
        tax_rate = Float
        discount_rate = Float
        construction_time = Float
    """

    #assembly.replace('fin_a', fin_csm_assembly())

    assembly.add('fixed_charge_rate', Float(0.12, iotype = 'in', desc = 'fixed charge rate for coe calculation'))
    assembly.add('construction_finance_rate', Float(0.00, iotype='in', desc = 'construction financing rate applied to overnight capital costs'))
    assembly.add('tax_rate', Float(0.4, iotype = 'in', desc = 'tax rate applied to operations'))
    assembly.add('discount_rate', Float(0.07, iotype = 'in', desc = 'applicable project discount rate'))
    assembly.add('construction_time', Float(1.0, iotype = 'in', desc = 'number of years to complete project construction'))

    # connections to fin
    assembly.connect('sea_depth', 'fin_a.sea_depth')
    assembly.connect('project_lifetime','fin_a.project_lifetime')
    assembly.connect('fixed_charge_rate','fin_a.fixed_charge_rate')
    assembly.connect('construction_finance_rate','fin_a.construction_finance_rate')
    assembly.connect('tax_rate','fin_a.tax_rate')
    assembly.connect('discount_rate','fin_a.discount_rate')
    assembly.connect('construction_time','fin_a.construction_time')


# =============================================================================
# Overall LCOE Assembly
@implement_base(ExtendedFinancialAnalysis)
class lcoe_se_assembly(Assembly):

    # Base I/O
    # Inputs
    turbine_number = Int(iotype = 'in', desc = 'number of turbines at plant')

    #Outputs
    turbine_cost = Float(iotype='out', desc = 'A Wind Turbine Capital _cost')
    bos_costs = Float(iotype='out', desc='A Wind Plant Balance of Station _cost Model')
    avg_annual_opex = Float(iotype='out', desc='A Wind Plant Operations Expenditures Model')
    net_aep = Float(iotype='out', desc='A Wind Plant Annual Energy Production Model', units='kW*h')
    coe = Float(iotype='out', desc='Levelized cost of energy for the wind plant')
    opex_breakdown = VarTree(OPEXVarTree(),iotype='out')
    bos_breakdown = VarTree(BOSVarTree(), iotype='out', desc='BOS cost breakdown')

    # Configuration options
    with_new_nacelle = Bool(False, iotype='in', desc='configure with DriveWPACT if false, else configure with DriveSE')
    with_landbose = Bool(False, iotype='in', desc='configure with CSM BOS if false, else configure with new LandBOS model')
    flexible_blade = Bool(False, iotype='in', desc='configure rotor with flexible blade if True')
    with_3pt_drive = Bool(False, iotype='in', desc='only used if configuring DriveSE - selects 3 pt or 4 pt design option') # TODO: change nacelle selection to enumerated rather than nested boolean
    with_ecn_opex = Bool(False, iotype='in', desc='configure with CSM OPEX if flase, else configure with ECN OPEX model')
    ecn_file = Str(iotype='in', desc='location of ecn excel file if used')

    # Other I/O needed at lcoe system level
    sea_depth = Float(0.0, units='m', iotype='in', desc='sea depth for offshore wind project')
    year = Int(2009, iotype='in', desc='year of project start')
    month = Int(12, iotype='in', desc='month of project start')
    project_lifetime = Float(20.0, iotype='in', desc = 'project lifetime for wind plant')

    def __init__(self, with_new_nacelle=False, with_landbos=False, flexible_blade=False, with_3pt_drive=False, with_ecn_opex=False, ecn_file=None):
        
        self.with_new_nacelle = with_new_nacelle
        self.with_landbos = with_landbos
        self.flexible_blade = flexible_blade
        self.with_3pt_drive = with_3pt_drive
        self.with_ecn_opex = with_ecn_opex
        if ecn_file == None:
            self.ecn_file=''
        else:
            self.ecn_file = ecn_file
        
        super(lcoe_se_assembly,self).__init__()

    def configure(self):
        """
        tcc_a inputs:
            advanced_blade = Bool
            offshore = Bool
            assemblyCostMultiplier = Float
            overheadCostMultiplier = Float
            profitMultiplier = Float
            transportMultiplier = Float
        aep inputs:
            array_losses = Float
            other_losses = Float
        fin inputs:
            fixed_charge_rate = Float
            construction_finance_rate = Float
            tax_rate = Float
            discount_rate = Float
            construction_time = Float
        bos inputs:
            bos_multiplier = Float
        inputs:
            sea_depth
            year
            month
            project lifetime
        if csm opex additional inputs:
            availability = Float()
        if openwind opex additional inputs:
            power_curve 
            rpm 
            ct 
        if with_landbos additional inputs:
            voltage
            distInter
            terrain
            layout
            soil
        """
    
        # configure base assembly
        configure_extended_financial_analysis(self)

        # putting replace statements here for now; TODO - openmdao bug
        # replace BOS with either CSM or landbos
        if self.with_landbos:
            self.replace('bos_a', NREL_Land_BOSSE())
        else:
            self.replace('bos_a', bos_csm_assembly())
        self.replace('tcc_a', Turbine_CostsSE())
        if self.with_ecn_opex:  
            self.replace('opex_a', opex_ecn_assembly(ecn_file))
        else:
            self.replace('opex_a', opex_csm_assembly())
        self.replace('aep_a', aep_weibull_assembly())
        self.replace('fin_a', fin_csm_assembly())
    
        # add TurbineSE assembly
        configure_turbine(self, self.with_new_nacelle, self.flexible_blade, self.with_3pt_drive)
    
        # replace TCC with turbine_costs
        configure_lcoe_with_turb_costs(self)
    
        # replace BOS with either CSM or landbos
        if self.with_landbos:
            configure_lcoe_with_landbos(self)
        else:
            configure_lcoe_with_csm_bos(self)

        # replace AEP with weibull AEP (TODO: option for basic aep)
        configure_lcoe_with_weibull_aep(self)
        
        # replace OPEX with CSM or ECN opex and add AEP
        if self.with_ecn_opex:  
            configure_lcoe_with_ecn_opex(self,ecn_file)     
            self.connect('opex_a.availability','aep_a.availability') # connecting here due to aep / opex reversal depending on model 
        else:
            configure_lcoe_with_csm_opex(self)
            self.add('availability',Float(0.94, iotype='in', desc='average annual availbility of wind turbines at plant'))
            self.connect('availability','aep_a.availability') # connecting here due to aep / opex reversal depending on model
    
        # replace Finance with CSM Finance
        configure_lcoe_with_csm_fin(self)


def create_example_se_assembly(wind_class='I',sea_depth=0.0,with_new_nacelle=False,with_landbos=False,flexible_blade=False,with_3pt_drive=False, with_ecn_opex=False, ecn_file=None, var_file=None, obj_file=None, con_file=None, with_openwind=False,ow_file=None,ow_wkbook=None):
    """
    Inputs:
        wind_class : str ('I', 'III', 'Offshore' - selected wind class for project)
        sea_depth : float (sea depth if an offshore wind plant)
    """

    # === Create LCOE SE assembly ========
    lcoe_se = lcoe_se_assembly(with_new_nacelle,with_landbos,flexible_blade,with_3pt_drive,with_ecn_opex,ecn_file)

    # === Set assembly variables and objects ===
    lcoe_se.sea_depth = sea_depth # 0.0 for land-based turbine
    lcoe_se.turbine_number = 100
    lcoe_se.year = 2009
    lcoe_se.month = 12

    rotor = lcoe_se.rotor
    nacelle = lcoe_se.nacelle
    tower = lcoe_se.tower
    tcc_a = lcoe_se.tcc_a
    # bos_a = lcoe_se.bos_a
    # opex_a = lcoe_se.opex_a
    aep_a = lcoe_se.aep_a
    fin_a = lcoe_se.fin_a

    # Turbine ===========
    from wisdem.reference_turbines.nrel5mw.nrel5mw import configure_nrel5mw_turbine
    configure_nrel5mw_turbine(lcoe_se,wind_class,lcoe_se.sea_depth)

    # tcc ====
    lcoe_se.advanced_blade = True
    lcoe_se.offshore = False
    lcoe_se.assemblyCostMultiplier = 0.30
    lcoe_se.profitMultiplier = 0.20
    lcoe_se.overheadCostMultiplier = 0.0
    lcoe_se.transportMultiplier = 0.0

    # for new landBOS
    # === new landBOS ===
    if with_landbos:
        lcoe_se.voltage = 137
        lcoe_se.distInter = 5
        lcoe_se.terrain = 'FLAT_TO_ROLLING'
        lcoe_se.layout = 'SIMPLE'
        lcoe_se.soil = 'STANDARD'

    # aep ==== # based on COE review for land-based machines
    if not with_openwind:
        lcoe_se.array_losses = 0.059
        #lcoe_se.A = 8.9 # weibull of 7.25 at 50 m with shear exp of 0.143
        lcoe_se.A = 10 * 2 / np.sqrt(np.pi) # weibull of 7.25 at 50 m with shear exp of 0.143 # changed by HF 20190827
        lcoe_se.k = 2.0
    lcoe_se.other_losses = 0.101
    if not with_ecn_opex:
        lcoe_se.availability = 0.94

    # fin ===
    lcoe_se.fixed_charge_rate = 0.095
    lcoe_se.construction_finance_rate = 0.0
    lcoe_se.tax_rate = 0.4
    lcoe_se.discount_rate = 0.07
    lcoe_se.construction_time = 1.0
    lcoe_se.project_lifetime = 20.0

    # Set plant level inputs ===
    shearExp = 0.2 #TODO : should be an input to lcoe
    #rotor.cdf_reference_height_wind_speed = 90.0
    if not with_openwind:
        lcoe_se.array_losses = 0.1
    lcoe_se.other_losses = 0.0
    if not with_ecn_opex:
        lcoe_se.availability = 0.98
    rotor.turbulence_class = 'B'
    lcoe_se.multiplier = 2.23

    if wind_class == 'Offshore':
        # rotor.cdf_reference_mean_wind_speed = 8.4 # TODO - aep from its own module
        # rotor.cdf_reference_height_wind_speed = 50.0
        # rotor.weibull_shape = 2.1
        shearExp = 0.14 # TODO : should be an input to lcoe
        lcoe_se.array_losses = 0.15
        if not with_ecn_opex:
            lcoe_se.availability = 0.96
        lcoe_se.offshore = True
        lcoe_se.multiplier = 2.33
        lcoe_se.fixed_charge_rate = 0.118

    rotor.shearExp = shearExp
    tower.wind1.shearExp = shearExp
    tower.wind2.shearExp = shearExp

    # ==== # ==== # ==== # ==== # ==== # ==== # ==== # ==== # ==== # ==== # ==== # ==== # ==== # ==== # ==== # ==== # ==== # ==== # ==== # ==== # ==== # ==== # ==== # ====
    # ==== # ==== # ==== # ==== # ==== # ==== # ==== # ==== # ==== # ==== # ==== # ==== # ==== # ==== # ==== # ==== # ==== # ==== # ==== # ==== # ==== # ==== # ==== # ====

    # === Run pop_vars assembly and print results
    variables = pd.read_csv(var_file, delimiter='\t', header=None).values # variables is numpy array
    npop = variables.shape[0]
    nvar = 32
    nobj = 5
    ncon = 22

    objectives = np.ones((npop, nobj)) * 1.0E+30
    constraints = np.ones((npop, ncon)) * -1.0E+30
    variableRanges = np.zeros((2, nvar))
    #DV 1 to 4
    variableRanges[0][0:4] = [1.0]*4
    variableRanges[1][0:4] = [5.3]*4
    #DV 5
    variableRanges[0][4] = 0.1
    variableRanges[1][4] = 0.3
    #DV 6 to 9
    variableRanges[0][5:9] = [-5.0]*4
    variableRanges[1][5:9] = [30.0]*4
    #DV 10 to 14
    variableRanges[0][9:14] = [0.005]*5
    variableRanges[1][9:14] = [0.200]*5
    #DV 15 to 19
    variableRanges[0][14:19] = [0.005]*5
    variableRanges[1][14:19] = [0.200]*5
    #DV 20 to 22
    variableRanges[0][19:22] = [-6.3]*3
    variableRanges[1][19:22] = [ 0.0]*3
    #DV 23 
    variableRanges[0][22] =  6.0
    variableRanges[1][22] = 14.0 
    #DV 24 
    variableRanges[0][23] =  6.0
    variableRanges[1][23] = 20.0 
    #DV 25 
    variableRanges[0][24] = 50.0
    variableRanges[1][24] = 80.0
    #DV 26
    variableRanges[0][25] = 20.0
    variableRanges[1][25] = 70.0
    #DV 27 to 29
    variableRanges[0][26:29] = [3.87]*3
    variableRanges[1][26:29] = [6.30]*3
    #DV 30 to 32
    variableRanges[0][29:32] = [0.005]*3
    variableRanges[1][29:32] = [0.100]*3

    for ipop in range(npop):
        print "evaluating %d th pop..." % (ipop)

        # --- Design Variables ---
        #
        ##DV 1 to 4
        lcoe_se.rotor.chord_sub = variableRanges[0][0:4] + (variableRanges[1][0:4] - variableRanges[0][0:4]) * variables[ipop][0:4] # (Array, m): chord at control points. defined at hub, then at linearly spaced locations from r_max_chord to tip
        ##DV 5 
        lcoe_se.rotor.r_max_chord = variableRanges[0][4] + (variableRanges[1][4] - variableRanges[0][4]) * variables[ipop][4]  # (Float): location of max chord on unit radius
        ##DV 6 to 9
        lcoe_se.rotor.theta_sub = variableRanges[0][5:9] + (variableRanges[1][5:9] - variableRanges[0][5:9]) * variables[ipop][5:9]   # (Array, deg): twist at control points.  defined at linearly spaced locations from r[idx_cylinder] to tip
        ##DV 10 to 14
        lcoe_se.rotor.sparT = variableRanges[0][9:14] + (variableRanges[1][9:14] - variableRanges[0][9:14]) * variables[ipop][9:14]   # (Array, m): spar cap thickness parameters
        ##DV 15 to 19
        lcoe_se.rotor.teT = variableRanges[0][14:19] + (variableRanges[1][14:19] - variableRanges[0][14:19]) * variables[ipop][14:19]   # (Array, m): trailing-edge thickness parameters
        ##DV 20 to 22
        #lcoe_se.rotor.precone = 0.0 # this is necessary for using precurve_sub 
        lcoe_se.rotor.precurve_sub = variableRanges[0][19:22] + (variableRanges[1][19:22] - variableRanges[0][19:22]) * variables[ipop][19:22]   # (Array, m): precurve at control points.  defined at same locations at chord, starting at 2nd control point (root must be zero precurve)
        ##DV 23
        lcoe_se.rotor.control.tsr = variableRanges[0][22] + (variableRanges[1][22] - variableRanges[0][22]) * variables[ipop][22]   # (Float): tip-speed ratio in Region 2 (should be optimized externally)
        ##DV 24
        lcoe_se.rotor.control.maxOmega = variableRanges[0][23] + (variableRanges[1][23] - variableRanges[0][23]) * variables[ipop][23]   # (Float, rpm): maximum allowed rotor rotation speed
        ##DV 25
        lcoe_se.rotor.bladeLength = variableRanges[0][24] + (variableRanges[1][24] - variableRanges[0][24]) * variables[ipop][24] 
        ##DV 26
        lcoe_se.tower.z_param[1] = variableRanges[0][25] + (variableRanges[1][25] - variableRanges[0][25]) * variables[ipop][25] 
        ##DV 27 to 29
        lcoe_se.tower_d = variableRanges[0][26:29] + (variableRanges[1][26:29] - variableRanges[0][26:29]) * variables[ipop][26:29] 
        ##DV 30 to 32
        lcoe_se.tower.t_param = variableRanges[0][29:32] + (variableRanges[1][29:32] - variableRanges[0][29:32]) * variables[ipop][29:32] 

        # === Run assembly
        lcoe_se.run()

        #MOP
        # --- Objectives ---
        AEP = lcoe_se.net_aep / lcoe_se.turbine_number
        #Objective 1: AEP per turbine
        iobj = 0
        objectives[ipop][iobj] = -AEP #maximization
        ##Objective 2: COE
        iobj = 1
        objectives[ipop][iobj] = lcoe_se.coe * AEP
        #Objective 3: Tower base load
        iobj = 2
        objectives[ipop][iobj] = lcoe_se.tower.tower1.Fx * lcoe_se.hub_height + lcoe_se.tower.tower1.Myy
        #Objective 4: Blade tip speed
        iobj = 3
        objectives[ipop][iobj] = lcoe_se.rotor.ratedConditions.Omega / 60.0 * (2.0 * np.pi) * (lcoe_se.rotor.diameter * 0.5)
        #Objective 5: Fatigue
        iobj = 4
        args = []
        args.append( max(lcoe_se.rotor.damageU_spar) )
        args.append( max(lcoe_se.rotor.damageL_spar) )
        args.append( max(lcoe_se.rotor.damageU_te) )
        args.append( max(lcoe_se.rotor.damageL_te) )
        args.append( np.log(max(lcoe_se.tower.damage)) )
        objectives[ipop][iobj] = max(args)

        # --- Constraints ---
        #Constraint 1: Tip deflection
        icon = 0
        value = lcoe_se.rotor.tip_deflection * 1.485
        threshold = lcoe_se.maxdeflection.max_tip_deflection
        constraints[ipop][icon] = threshold - value
        #Constraint 2: Ground clearance
        icon = 1
        value = lcoe_se.maxdeflection.ground_clearance
        threshold = 20.0
        constraints[ipop][icon] = value - threshold
        #Constraint 3: Blade eigen frequency
        icon = 2
        value = lcoe_se.rotor.freq[0]
        threshold = 1.1* lcoe_se.rotor.ratedConditions.Omega / 60.0* lcoe_se.rotor.nBlades
        constraints[ipop][icon] = value - threshold
        #Constraint 4: Tower eigen frequency
        icon = 3
        value = lcoe_se.tower.f1
        threshold = 1.1* lcoe_se.rotor.ratedConditions.Omega / 60.0
        constraints[ipop][icon] = value - threshold
        #Constraint 5: Tower 1 stress
        icon = 4
        value = max(lcoe_se.tower.stress1)
        threshold = 1.0
        constraints[ipop][icon] = threshold - value
        #Constraint 6: Tower 2 stress
        icon = 5
        value = max(lcoe_se.tower.stress2)
        threshold = 1.0
        constraints[ipop][icon] = threshold - value
        #Constraint 7: Tower 1 global buckling 
        icon = 6
        value = max(lcoe_se.tower.global_buckling1)
        threshold = 1.0
        constraints[ipop][icon] = threshold - value
        #Constraint 8: Tower 1 global buckling 
        icon = 7
        value = max(lcoe_se.tower.global_buckling2)
        threshold = 1.0
        constraints[ipop][icon] = threshold - value
        #Constraint 9: Tower 1 shell buckling 
        icon = 8
        value = max(lcoe_se.tower.shell_buckling1)
        threshold = 1.0
        constraints[ipop][icon] = threshold - value
        #Constraint 10: Tower 2 shell buckling 
        icon = 9
        value = max(lcoe_se.tower.shell_buckling2)
        threshold = 1.0
        constraints[ipop][icon] = threshold - value
        #Constraint 11: Tower 1 fatigue
        icon = 10
        value = max(lcoe_se.tower.damage)
        threshold = 1.0
        constraints[ipop][icon] = threshold - value
        #Constraint 12: Manufacturability
        icon = 11
        value = max(lcoe_se.tower.manufacturability)
        threshold = 0.0
        constraints[ipop][icon] = threshold - value
        #Constraint 13: Weldability
        icon = 12
        value = max(lcoe_se.tower.weldability)
        threshold = 0.0
        constraints[ipop][icon] = threshold - value
        #Constraint 14: Blade tip speed
        icon = 13
        value = lcoe_se.rotor.ratedConditions.Omega / 60.0 * (2.0 * np.pi) * (lcoe_se.rotor.diameter * 0.5)
        threshold = 80.0
        constraints[ipop][icon] = threshold - value
        #Constraint 15: Blade buckling for spar
        icon = 14
        value = np.min([lcoe_se.rotor.strainU_spar, lcoe_se.rotor.strainL_spar], axis=0)
        threshold = lcoe_se.rotor.eps_crit_spar
        constraints[ipop][icon] = min(value - threshold)
        #Constraint 16: Blade buckling for te
        icon = 15
        value = np.min([lcoe_se.rotor.strainU_te, lcoe_se.rotor.strainL_te], axis=0)
        threshold = lcoe_se.rotor.eps_crit_te
        constraints[ipop][icon] = min(value - threshold)
        #Constraint 17: Blade fatigue
        icon = 16
        args = []
        args.append( max(lcoe_se.rotor.damageU_spar) )
        args.append( max(lcoe_se.rotor.damageL_spar) )
        args.append( max(lcoe_se.rotor.damageU_te) )
        args.append( max(lcoe_se.rotor.damageL_te) )
        value = max(args)
        threshold = 0.0
        constraints[ipop][icon] = threshold - value
        #Constraint 18: Blade strain spar
        icon = 17
        value = min( min(lcoe_se.rotor.strainU_spar), min(lcoe_se.rotor.strainL_spar) )
        threshold = -0.01 / 1.755
        constraints[ipop][icon] = value - threshold
        #Constraint 19: Blade strain spar
        icon = 18
        value = max( max(lcoe_se.rotor.strainU_spar), max(lcoe_se.rotor.strainL_spar) )
        threshold = 0.01 / 1.755
        constraints[ipop][icon] = threshold - value
        #Constraint 20: Blade strain te
        icon = 19
        value = min( min(lcoe_se.rotor.strainU_te), min(lcoe_se.rotor.strainL_te) )
        threshold = -0.0025 / 1.755
        constraints[ipop][icon] = value - threshold
        #Constraint 21: Blade strain te
        icon = 20
        value = min( min(lcoe_se.rotor.strainU_te), min(lcoe_se.rotor.strainL_te) )
        threshold = 0.0025 / 1.755
        constraints[ipop][icon] = threshold - value
        #Constraint 22: AEP
        icon = 21
        value = AEP
        threshold = 0.0
        constraints[ipop][icon] = value - threshold


    pd.DataFrame(objectives).to_csv(obj_file, sep='\t', index=False, header=False)
    pd.DataFrame(constraints).to_csv(con_file, sep='\t', index=False, header=False)


if __name__ == '__main__':

    # NREL 5 MW in land-based wind plant with high winds (as class I)
    wind_class = 'I'
    sea_depth = 0.0
    with_new_nacelle = True
    with_landbos = False
    flexible_blade = False
    with_3pt_drive = False
    with_ecn_opex = False
    ecn_file = ''

    interdir=''
    args=sys.argv
    if len(args) != 2:
        print 'Error: %s takes exactly one argument' % args[0]
        sys.exit()
    else:
        interdir = args[1]
        if not interdir.endswith(os.sep):
            interdir += os.sep
    var_file = interdir + 'pop_vars_eval.txt'
    obj_file = interdir + 'pop_objs_eval.txt'
    con_file = interdir + 'pop_cons_eval.txt'

    create_example_se_assembly(wind_class,sea_depth,with_new_nacelle,with_landbos,flexible_blade,with_3pt_drive,with_ecn_opex,ecn_file,var_file,obj_file,con_file) 
   
    
