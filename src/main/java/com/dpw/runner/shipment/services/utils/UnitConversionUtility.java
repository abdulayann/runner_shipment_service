package com.dpw.runner.shipment.services.utils;

import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import tec.units.ri.unit.MetricPrefix;
import tec.units.ri.unit.Units;

import javax.measure.Unit;
import javax.measure.UnitConverter;
import javax.measure.quantity.Length;
import javax.measure.quantity.Mass;
import javax.measure.quantity.Volume;
import java.math.BigDecimal;

import static com.dpw.runner.shipment.services.utils.CommonUtils.IsStringNullOrEmpty;

@Slf4j
@Component
public class UnitConversionUtility {
    private UnitConversionUtility(){}
    public static Number convertUnit(String type, BigDecimal value, String fromUnit, String toUnit) throws RunnerException {
        String responseMsg;
        try {
            if(value == null)
                return 0;
            if(IsStringNullOrEmpty(fromUnit) || IsStringNullOrEmpty(toUnit)) {
                return value;
            }
            Unit<?> sourceUnit = getUnitType(type, fromUnit);
            Unit<?> targetUnit = getUnitType(type, toUnit);
            UnitConverter converter = sourceUnit.getConverterToAny(targetUnit);
            return converter.convert(value);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_CALCULATION_ERROR;
            log.error(responseMsg, e);
            throw new RunnerException(e.getMessage());
        }
    }

    static Unit<?> getUnitType(String type, String unitSymbol) {
        switch (type) {
            case Constants.MASS:
                return getWeightUnitForSymbol(unitSymbol);
            case Constants.VOLUME:
                return getVolumeUnitForSymbol(unitSymbol);
            case Constants.LENGTH:
                return getLengthUnitForSymbol(unitSymbol);
            default:
                throw new IllegalArgumentException(DaoConstants.DAO_UNKNOWN_UNIT + type);
        }
    }

    static Unit<Length> getLengthUnitForSymbol(String unitSymbol) {
        switch (unitSymbol) {
            case Constants.METRE:
                return Units.METRE;
            case Constants.CENTI:
                return MetricPrefix.CENTI(Units.METRE);
            case Constants.DECI:
                return MetricPrefix.DECI(Units.METRE);
            case Constants.INCH:
                return Units.METRE.multiply(0.0254);
            case Constants.ANGSTROM:
                return Units.METRE.multiply(1E-10);
            case Constants.ASTRONOMICAL_UNIT:
                return Units.METRE.multiply(1.49598E11);
            case Constants.CENTILEAGUE:
                return Units.METRE.multiply(0.000254);
            case Constants.KILOMETER:
                return Units.METRE.multiply(1000);
            case Constants.ELL:
                return Units.METRE.multiply(1.143);
            case Constants.EXAMETER:
                return Units.METRE.multiply(4.2323E-03);
            case Constants.FATHOM:
                return Units.METRE.multiply(1.8288);
            case Constants.FURLONG:
                return Units.METRE.multiply(201.168);
            case Constants.FOOT, Constants.FOOT_FT:
                return Units.METRE.multiply(0.3048);
            case Constants.LI:
                return Units.METRE.multiply(5556);
            case Constants.LIGHT_YEAR:
                return Units.METRE.multiply(9.46055E+15);
            case Constants.MICRO_METER:
                return Units.METRE.multiply(1E-6);
            case Constants.MIL:
                return Units.METRE.multiply(0.0000254);
            case Constants.MILLIMETER:
                return Units.METRE.multiply(0.001);
            case Constants.NANOMETER:
                return Units.METRE.multiply(1E-9);
            case Constants.NAUTICAL_MILE:
                return Units.METRE.multiply(1852);
            case Constants.MICROINCH:
                return Units.METRE.multiply(1E-12);
            case Constants.MILLIINCH:
                return Units.METRE.multiply(0.0003514598);
            case Constants.PARSEC:
                return Units.METRE.multiply(3.08374E+16);
            case Constants.PICA:
                return Units.METRE.multiply(4.217518E-03);
            case Constants.PICOMETER:
                return Units.METRE.multiply(1E-12);
            case Constants.POINT:
                return Units.METRE.multiply(0.0003514598);
            case Constants.ROD:
                return Units.METRE.multiply(5.0292);
            case Constants.YARD:
                return Units.METRE.multiply(0.9144);
            default:
                throw new IllegalArgumentException(DaoConstants.DAO_UNKNOWN_UNIT + unitSymbol);
        }
    }

    static Unit<Mass> getWeightUnitForSymbol(String unitSymbol) {
        switch (unitSymbol) {
            case Constants.WEIGHT_UNIT_KG:
                return MetricPrefix.KILO(Units.GRAM);
            case Constants.WEIGHT_UNIT_GRAM:
                return Units.GRAM;
            case Constants.METRIC_TON:
                return MetricPrefix.MEGA(Units.GRAM);
            case Constants.WEIGHT_UNIT_MG:
                return Units.GRAM.multiply(0.001);
            case Constants.WEIGHT_UNIT_UG:
                return Units.GRAM.multiply(0.000001);
            case Constants.WEIGHT_UNIT_MC:
                return Units.GRAM.multiply(0.2);
            case Constants.WEIGHT_UNIT_HUNDRED_WT_LONG:
                return Units.GRAM.multiply(50802.35);
            case Constants.WEIGHT_UNIT_HUNDRED_WT_SHORT:
                return MetricPrefix.KILO(Units.GRAM).multiply(45.35924);
            case Constants.WEIGHT_UNIT_LB:
                return MetricPrefix.KILO(Units.GRAM).multiply(0.4535924);
            case Constants.WEIGHT_UNIT_LT:
                return MetricPrefix.KILO(Units.GRAM).multiply(0.3732417);
            case Constants.WEIGHT_UNIT_OZ:
                return MetricPrefix.KILO(Units.GRAM).multiply(0.02834952);
            case Constants.WEIGHT_UNIT_OT:
                return MetricPrefix.KILO(Units.GRAM).multiply(0.03110348);
            case Constants.WEIGHT_UNIT_Slug:
                return MetricPrefix.KILO(Units.GRAM).multiply(14.5939);
            case Constants.WEIGHT_UNIT_TA:
                return MetricPrefix.KILO(Units.GRAM).multiply(0.02916667);
            case Constants.WEIGHT_UNIT_TL:
                return MetricPrefix.KILO(Units.GRAM).multiply(1016.047);
            case Constants.WEIGHT_UNIT_TN:
                return MetricPrefix.KILO(Units.GRAM).multiply(907.1847);
            case Constants.WEIGHT_UNIT_TM:
            case Constants.WEIGHT_UNIT_T:
                return MetricPrefix.KILO(Units.GRAM).multiply(1000);
            case Constants.WEIGHT_UNIT_HG:
                return MetricPrefix.KILO(Units.GRAM).multiply(0.10);
            case Constants.WEIGHT_UNIT_KT:
                return MetricPrefix.KILO(Units.GRAM).multiply(1000000);
            case Constants.WEIGHT_UNIT_DT:
                return MetricPrefix.KILO(Units.GRAM).multiply(100);
            default:
                throw new IllegalArgumentException(DaoConstants.DAO_UNKNOWN_UNIT + unitSymbol);
        }
    }

    static Unit<Volume> getVolumeUnitForSymbol(String unitSymbol) {
        switch (unitSymbol) {
            case Constants.VOLUME_UNIT_M3:
            case Constants.VOLUME_UNIT_Stere:
            case Constants.VOLUME_UNIT_CBM:
                return Units.CUBIC_METRE;
            case Constants.VOLUME_UNIT_LITRE:
                return Units.LITRE;
            case Constants.VOLUME_UNIT_CC:
                return Units.CUBIC_METRE.multiply(0.000001);
            case Constants.VOLUME_UNIT_CM:
                return Units.CUBIC_METRE.multiply(0.000000001);
            case Constants.VOLUME_UNIT_AF:
                return Units.CUBIC_METRE.multiply(1233.482);
            case Constants.VOLUME_UNIT_Barrel_OIL:
                return Units.CUBIC_METRE.multiply(0.1589873);
            case Constants.VOLUME_UNIT_Board_foot:
                return Units.CUBIC_METRE.multiply(0.002359737);
            case Constants.VOLUME_UNIT_Bushel_US:
                return Units.CUBIC_METRE.multiply(0.03523907);
            case Constants.VOLUME_UNIT_Cup:
                return Units.CUBIC_METRE.multiply(0.0002365882);
            case Constants.VOLUME_UNIT_Fluid_OUNCE_US:
            case Constants.VOLUME_UNIT_Ounce_US_FLD:
                return Units.CUBIC_METRE.multiply(0.00002957353);
            case Constants.VOLUME_UNIT_CF:
                return Units.CUBIC_METRE.multiply(0.02831685);
            case Constants.VOLUME_UNIT_GI:
                return Units.CUBIC_METRE.multiply(0.004546087);
            case Constants.VOLUME_UNIT_GA:
                return Units.CUBIC_METRE.multiply(0.004404884);
            case Constants.VOLUME_UNIT_Gallon_US_LIQ:
                return Units.CUBIC_METRE.multiply(.003785412);
            case Constants.VOLUME_UNIT_Gill_UK:
                return Units.CUBIC_METRE.multiply(0.0001420652);
            case Constants.VOLUME_UNIT_Gill_US:
                return Units.CUBIC_METRE.multiply(0.0001182941);
            case Constants.VOLUME_UNIT_CI:
                return Units.CUBIC_METRE.multiply(0.00001638706);
            case Constants.VOLUME_UNIT_Liter_OLD:
                return Units.CUBIC_METRE.multiply(0.001000028);
            case Constants.VOLUME_UNIT_Ounce_UK_FLD:
                return Units.CUBIC_METRE.multiply(0.00002841305);
            case Constants.VOLUME_UNIT_Peck_US:
                return Units.CUBIC_METRE.multiply(8.8097680E-03);
            case Constants.VOLUME_UNIT_Pint_US_DRY:
                return Units.CUBIC_METRE.multiply(0.0005506105);
            case Constants.VOLUME_UNIT_Pint_US_LIQ:
                return Units.CUBIC_METRE.multiply(4.7317650E-04);
            case Constants.VOLUME_UNIT_Quart_US_DRY:
                return Units.CUBIC_METRE.multiply(0.001101221);
            case Constants.VOLUME_UNIT_Quart_US_LIQ:
                return Units.CUBIC_METRE.multiply(9.46353E-04);
            case Constants.VOLUME_UNIT_Tablespoon:
                return Units.CUBIC_METRE.multiply(0.00001478676);
            case Constants.VOLUME_UNIT_Teaspoon:
                return Units.CUBIC_METRE.multiply(0.000004928922);
            case Constants.VOLUME_UNIT_TON_REGISTER:
                return Units.CUBIC_METRE.multiply(2.831685);
            case Constants.VOLUME_UNIT_CY:
                return Units.CUBIC_METRE.multiply(0.7645549);
            case Constants.VOLUME_UNIT_ML:
                return Units.CUBIC_METRE.multiply(1000);
            case Constants.VOLUME_UNIT_D3:
                return Units.CUBIC_METRE.multiply(0.001);
            default:
                throw new IllegalArgumentException(DaoConstants.DAO_UNKNOWN_UNIT + unitSymbol);
        }
    }
}
