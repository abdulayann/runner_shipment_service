package com.dpw.runner.shipment.services.utils;

import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import tec.units.ri.unit.MetricPrefix;
import tec.units.ri.unit.Units;

import javax.measure.Unit;
import java.math.BigDecimal;

import static com.dpw.runner.shipment.services.commons.constants.Constants.*;
import static com.dpw.runner.shipment.services.commons.constants.DaoConstants.DAO_UNKNOWN_UNIT;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThrows;

 class UnitConversionUtilityTest {

    @InjectMocks
    private UnitConversionUtility unitConversionUtility;

    @Test
     void testConvertUnit_WithValidInputs() throws RunnerException {
        BigDecimal value = BigDecimal.valueOf(1000);
        String fromUnit = WEIGHT_UNIT_KG;
        String toUnit = WEIGHT_UNIT_GRAM;
        String type = MASS;

        Number result = UnitConversionUtility.convertUnit(type, value, fromUnit, toUnit);
        assertEquals(1000000.0, result);
    }

    @Test
     void testConvertUnit_NullValue() throws RunnerException {
        BigDecimal value = null;
        String fromUnit = WEIGHT_UNIT_KG;
        String toUnit = WEIGHT_UNIT_GRAM;
        String type = MASS;

        Number result = UnitConversionUtility.convertUnit(type, value, fromUnit, toUnit);
        assertEquals(0, result);
    }

    @Test
     void testConvertUnit_EmptyFromUnit() throws RunnerException {
        BigDecimal value = BigDecimal.valueOf(1000);
        String fromUnit = "";
        String toUnit = WEIGHT_UNIT_GRAM;
        String type = MASS;

        Number result = UnitConversionUtility.convertUnit(type, value, fromUnit, toUnit);
        assertEquals(value, result);
    }

    @Test
     void testConvertUnit_EmptyToUnit() throws RunnerException {
        BigDecimal value = BigDecimal.valueOf(1000);
        String fromUnit = WEIGHT_UNIT_KG;
        String toUnit = "";
        String type = MASS;

        Number result = UnitConversionUtility.convertUnit(type, value, fromUnit, toUnit);
        assertEquals(value, result);
    }

    @Test
     void testConvertUnit_InvalidUnit() {
        BigDecimal value = BigDecimal.valueOf(1000);
        String fromUnit = "invalid";
        String toUnit = WEIGHT_UNIT_GRAM;
        String type = MASS;

        RunnerException thrown = assertThrows(RunnerException.class, () -> {
            UnitConversionUtility.convertUnit(type, value, fromUnit, toUnit);
        });

        assertEquals("Unknown unit : invalid", thrown.getMessage());
    }

    @Test
     void testConvertUnit_InvalidType() {
        BigDecimal value = BigDecimal.valueOf(1000);
        String fromUnit = WEIGHT_UNIT_KG;
        String toUnit = WEIGHT_UNIT_GRAM;
        String type = "invalid";

        RunnerException thrown = assertThrows(RunnerException.class, () -> {
            UnitConversionUtility.convertUnit(type, value, fromUnit, toUnit);
        });

        assertEquals("Unknown unit : invalid", thrown.getMessage());
    }

    @Test
     void testConvertUnit_ExceptionHandling() {
        BigDecimal value = BigDecimal.valueOf(1000);
        String fromUnit = WEIGHT_UNIT_KG;
        String toUnit = WEIGHT_UNIT_GRAM;
        String type = MASS;

        RunnerException thrown = assertThrows(RunnerException.class, () -> {
            UnitConversionUtility.convertUnit(type, value, "invalid", toUnit);
        });

        assertEquals("Unknown unit : invalid", thrown.getMessage());
    }

    @Test
     void testGetUnitType_Mass() {
        Unit<?> unit = UnitConversionUtility.getUnitType(MASS, WEIGHT_UNIT_KG);
        assertEquals(MetricPrefix.KILO(Units.GRAM), unit);
    }

    @Test
     void testGetUnitType_Length() {
        Unit<?> unit = UnitConversionUtility.getUnitType(LENGTH, METRE);
        assertEquals(Units.METRE, unit);
    }

    @Test
     void testGetUnitType_Volume() {
        Unit<?> unit = UnitConversionUtility.getUnitType(VOLUME, VOLUME_UNIT_LITRE);
        assertEquals(Units.LITRE, unit);
    }

    @Test
     void testGetUnitType_InvalidType() {
        IllegalArgumentException thrown = assertThrows(IllegalArgumentException.class, () -> {
            UnitConversionUtility.getUnitType("invalid", METRE);
        });

        assertEquals(DAO_UNKNOWN_UNIT + "invalid", thrown.getMessage());
    }

    @Test
     void testGetLengthUnitForSymbol() {
        assertEquals(Units.METRE, UnitConversionUtility.getLengthUnitForSymbol(METRE));
        assertEquals(MetricPrefix.CENTI(Units.METRE), UnitConversionUtility.getLengthUnitForSymbol(CENTI));
        assertEquals(MetricPrefix.DECI(Units.METRE), UnitConversionUtility.getLengthUnitForSymbol(DECI));
        assertEquals(Units.METRE.multiply(0.0254), UnitConversionUtility.getLengthUnitForSymbol(INCH));
        assertEquals(Units.METRE.multiply(1E-10), UnitConversionUtility.getLengthUnitForSymbol(ANGSTROM));
        assertEquals(Units.METRE.multiply(1.49598E11), UnitConversionUtility.getLengthUnitForSymbol(ASTRONOMICAL_UNIT));
        assertEquals(Units.METRE.multiply(0.000254), UnitConversionUtility.getLengthUnitForSymbol(CENTILEAGUE));
        assertEquals(Units.METRE.multiply(1000), UnitConversionUtility.getLengthUnitForSymbol(KILOMETER));
        assertEquals(Units.METRE.multiply(1.143), UnitConversionUtility.getLengthUnitForSymbol(ELL));
        assertEquals(Units.METRE.multiply(4.2323E-03), UnitConversionUtility.getLengthUnitForSymbol(EXAMETER));
        assertEquals(Units.METRE.multiply(1.8288), UnitConversionUtility.getLengthUnitForSymbol(FATHOM));
        assertEquals(Units.METRE.multiply(201.168), UnitConversionUtility.getLengthUnitForSymbol(FURLONG));
        assertEquals(Units.METRE.multiply(0.3048), UnitConversionUtility.getLengthUnitForSymbol(FOOT));
        assertEquals(Units.METRE.multiply(0.3048), UnitConversionUtility.getLengthUnitForSymbol(FOOT_FT));
        assertEquals(Units.METRE.multiply(5556), UnitConversionUtility.getLengthUnitForSymbol(LI));
        assertEquals(Units.METRE.multiply(9.46055E+15), UnitConversionUtility.getLengthUnitForSymbol(LIGHT_YEAR));
        assertEquals(Units.METRE.multiply(1E-6), UnitConversionUtility.getLengthUnitForSymbol(MICRO_METER));
        assertEquals(Units.METRE.multiply(0.0000254), UnitConversionUtility.getLengthUnitForSymbol(MIL));
        assertEquals(Units.METRE.multiply(0.001), UnitConversionUtility.getLengthUnitForSymbol(MILLIMETER));
        assertEquals(Units.METRE.multiply(1E-9), UnitConversionUtility.getLengthUnitForSymbol(NANOMETER));
        assertEquals(Units.METRE.multiply(1852), UnitConversionUtility.getLengthUnitForSymbol(NAUTICAL_MILE));
        assertEquals(Units.METRE.multiply(1E-12), UnitConversionUtility.getLengthUnitForSymbol(MICROINCH));
        assertEquals(Units.METRE.multiply(0.0003514598), UnitConversionUtility.getLengthUnitForSymbol(MILLIINCH));
        assertEquals(Units.METRE.multiply(3.08374E+16), UnitConversionUtility.getLengthUnitForSymbol(PARSEC));
        assertEquals(Units.METRE.multiply(4.217518E-03), UnitConversionUtility.getLengthUnitForSymbol(PICA));
        assertEquals(Units.METRE.multiply(1E-12), UnitConversionUtility.getLengthUnitForSymbol(PICOMETER));
        assertEquals(Units.METRE.multiply(0.0003514598), UnitConversionUtility.getLengthUnitForSymbol(POINT));
        assertEquals(Units.METRE.multiply(5.0292), UnitConversionUtility.getLengthUnitForSymbol(ROD));
        assertEquals(Units.METRE.multiply(0.9144), UnitConversionUtility.getLengthUnitForSymbol(YARD));
    }
//
//    @Test
//     void testGetWeightUnitForSymbol() {
//        assertEquals(MetricPrefix.KILO(Units.GRAM), UnitConversionUtility.getWeightUnitForSymbol(WEIGHT_UNIT_KG));
//        assertEquals(Units.GRAM, UnitConversionUtility.getWeightUnitForSymbol(WEIGHT_UNIT_GRAM));
//        assertEquals(MetricPrefix.MEGA(Units.GRAM), UnitConversionUtility.getWeightUnitForSymbol(METRIC_TON));
//        assertEquals(Units.GRAM.multiply(0.001), UnitConversionUtility.getWeightUnitForSymbol(WEIGHT_UNIT_MG));
//        assertEquals(Units.GRAM.multiply(0.000001), UnitConversionUtility.getWeightUnitForSymbol(WEIGHT_UNIT_UG));
//        assertEquals(Units.GRAM.multiply(0.000000001), UnitConversionUtility.getWeightUnitForSymbol(WEIGHT_UNIT_NG));
//        assertEquals(Units.GRAM.multiply(28.3495), UnitConversionUtility.getWeightUnitForSymbol(OUNCE));
//        assertEquals(Units.GRAM.multiply(453.592), UnitConversionUtility.getWeightUnitForSymbol(POUND));
//        assertEquals(Units.GRAM.multiply(373.242), UnitConversionUtility.getWeightUnitForSymbol(TROY_OUNCE));
//        assertEquals(Units.GRAM.multiply(907185), UnitConversionUtility.getWeightUnitForSymbol(TON));
//        assertEquals(Units.GRAM.multiply(1000), UnitConversionUtility.getWeightUnitForSymbol(QUINTAL));
//        assertEquals(Units.GRAM.multiply(0.2), UnitConversionUtility.getWeightUnitForSymbol(CARAT));
//    }

    @Test
     void testGetWeightUnitForSymbol() {
        assertEquals(MetricPrefix.KILO(Units.GRAM), UnitConversionUtility.getWeightUnitForSymbol(WEIGHT_UNIT_KG));
        assertEquals(Units.GRAM, UnitConversionUtility.getWeightUnitForSymbol(WEIGHT_UNIT_GRAM));
        assertEquals(MetricPrefix.MEGA(Units.GRAM), UnitConversionUtility.getWeightUnitForSymbol(METRIC_TON));
        assertEquals(Units.GRAM.multiply(0.001), UnitConversionUtility.getWeightUnitForSymbol(WEIGHT_UNIT_MG));
        assertEquals(Units.GRAM.multiply(0.000001), UnitConversionUtility.getWeightUnitForSymbol(WEIGHT_UNIT_UG));
        assertEquals(Units.GRAM.multiply(0.2), UnitConversionUtility.getWeightUnitForSymbol(WEIGHT_UNIT_MC));
        assertEquals(Units.GRAM.multiply(50802.35), UnitConversionUtility.getWeightUnitForSymbol(WEIGHT_UNIT_HUNDRED_WT_LONG));
        assertEquals(MetricPrefix.KILO(Units.GRAM).multiply(45.35924), UnitConversionUtility.getWeightUnitForSymbol(WEIGHT_UNIT_HUNDRED_WT_SHORT));
        assertEquals(MetricPrefix.KILO(Units.GRAM).multiply(0.4535924), UnitConversionUtility.getWeightUnitForSymbol(WEIGHT_UNIT_LB));
        assertEquals(MetricPrefix.KILO(Units.GRAM).multiply(0.3732417), UnitConversionUtility.getWeightUnitForSymbol(WEIGHT_UNIT_LT));
        assertEquals(MetricPrefix.KILO(Units.GRAM).multiply(0.02834952), UnitConversionUtility.getWeightUnitForSymbol(WEIGHT_UNIT_OZ));
        assertEquals(MetricPrefix.KILO(Units.GRAM).multiply(0.03110348), UnitConversionUtility.getWeightUnitForSymbol(WEIGHT_UNIT_OT));
        assertEquals(MetricPrefix.KILO(Units.GRAM).multiply(14.5939), UnitConversionUtility.getWeightUnitForSymbol(WEIGHT_UNIT_Slug));
        assertEquals(MetricPrefix.KILO(Units.GRAM).multiply(0.02916667), UnitConversionUtility.getWeightUnitForSymbol(WEIGHT_UNIT_TA));
        assertEquals(MetricPrefix.KILO(Units.GRAM).multiply(1016.047), UnitConversionUtility.getWeightUnitForSymbol(WEIGHT_UNIT_TL));
        assertEquals(MetricPrefix.KILO(Units.GRAM).multiply(907.1847), UnitConversionUtility.getWeightUnitForSymbol(WEIGHT_UNIT_TN));
        assertEquals(MetricPrefix.KILO(Units.GRAM).multiply(1000), UnitConversionUtility.getWeightUnitForSymbol(WEIGHT_UNIT_TM));
        assertEquals(MetricPrefix.KILO(Units.GRAM).multiply(1000), UnitConversionUtility.getWeightUnitForSymbol(WEIGHT_UNIT_T));
        assertEquals(MetricPrefix.KILO(Units.GRAM).multiply(0.10), UnitConversionUtility.getWeightUnitForSymbol(WEIGHT_UNIT_HG));
        assertEquals(MetricPrefix.KILO(Units.GRAM).multiply(1000000), UnitConversionUtility.getWeightUnitForSymbol(WEIGHT_UNIT_KT));
        assertEquals(MetricPrefix.KILO(Units.GRAM).multiply(100), UnitConversionUtility.getWeightUnitForSymbol(WEIGHT_UNIT_DT));
    }

     @Test
     public void testGetVolumeUnitForSymbol() {
         assertEquals(Units.CUBIC_METRE, UnitConversionUtility.getVolumeUnitForSymbol(VOLUME_UNIT_M3));
         assertEquals(Units.CUBIC_METRE, UnitConversionUtility.getVolumeUnitForSymbol(VOLUME_UNIT_Stere));
         assertEquals(Units.CUBIC_METRE, UnitConversionUtility.getVolumeUnitForSymbol(VOLUME_UNIT_CBM));
         assertEquals(Units.LITRE, UnitConversionUtility.getVolumeUnitForSymbol(VOLUME_UNIT_LITRE));
         assertEquals(Units.CUBIC_METRE.multiply(0.000001), UnitConversionUtility.getVolumeUnitForSymbol(VOLUME_UNIT_CC));
         assertEquals(Units.CUBIC_METRE.multiply(0.000000001), UnitConversionUtility.getVolumeUnitForSymbol(VOLUME_UNIT_CM));
         assertEquals(Units.CUBIC_METRE.multiply(1233.482), UnitConversionUtility.getVolumeUnitForSymbol(VOLUME_UNIT_AF));
         assertEquals(Units.CUBIC_METRE.multiply(0.1589873), UnitConversionUtility.getVolumeUnitForSymbol(VOLUME_UNIT_Barrel_OIL));
         assertEquals(Units.CUBIC_METRE.multiply(0.002359737), UnitConversionUtility.getVolumeUnitForSymbol(VOLUME_UNIT_Board_foot));
         assertEquals(Units.CUBIC_METRE.multiply(0.03523907), UnitConversionUtility.getVolumeUnitForSymbol(VOLUME_UNIT_Bushel_US));
         assertEquals(Units.CUBIC_METRE.multiply(0.0002365882), UnitConversionUtility.getVolumeUnitForSymbol(VOLUME_UNIT_Cup));
         assertEquals(Units.CUBIC_METRE.multiply(0.00002957353), UnitConversionUtility.getVolumeUnitForSymbol(VOLUME_UNIT_Fluid_OUNCE_US));
         assertEquals(Units.CUBIC_METRE.multiply(0.02831685), UnitConversionUtility.getVolumeUnitForSymbol(VOLUME_UNIT_CF));
         assertEquals(Units.CUBIC_METRE.multiply(0.004546087), UnitConversionUtility.getVolumeUnitForSymbol(VOLUME_UNIT_GI));
         assertEquals(Units.CUBIC_METRE.multiply(0.004404884), UnitConversionUtility.getVolumeUnitForSymbol(VOLUME_UNIT_GA));
         assertEquals(Units.CUBIC_METRE.multiply(.003785412), UnitConversionUtility.getVolumeUnitForSymbol(VOLUME_UNIT_Gallon_US_LIQ));
         assertEquals(Units.CUBIC_METRE.multiply(0.0001420652), UnitConversionUtility.getVolumeUnitForSymbol(VOLUME_UNIT_Gill_UK));
         assertEquals(Units.CUBIC_METRE.multiply(0.0001182941), UnitConversionUtility.getVolumeUnitForSymbol(VOLUME_UNIT_Gill_US));
         assertEquals(Units.CUBIC_METRE.multiply(0.00001638706), UnitConversionUtility.getVolumeUnitForSymbol(VOLUME_UNIT_CI));
         assertEquals(Units.CUBIC_METRE.multiply(0.001000028), UnitConversionUtility.getVolumeUnitForSymbol(VOLUME_UNIT_Liter_OLD));
         assertEquals(Units.CUBIC_METRE.multiply(0.00002841305), UnitConversionUtility.getVolumeUnitForSymbol(VOLUME_UNIT_Ounce_UK_FLD));
         assertEquals(Units.CUBIC_METRE.multiply(8.8097680E-03), UnitConversionUtility.getVolumeUnitForSymbol(VOLUME_UNIT_Peck_US));
         assertEquals(Units.CUBIC_METRE.multiply(0.0005506105), UnitConversionUtility.getVolumeUnitForSymbol(VOLUME_UNIT_Pint_US_DRY));
         assertEquals(Units.CUBIC_METRE.multiply(4.7317650E-04), UnitConversionUtility.getVolumeUnitForSymbol(VOLUME_UNIT_Pint_US_LIQ));
         assertEquals(Units.CUBIC_METRE.multiply(0.001101221), UnitConversionUtility.getVolumeUnitForSymbol(VOLUME_UNIT_Quart_US_DRY));
         assertEquals(Units.CUBIC_METRE.multiply(9.46353E-04), UnitConversionUtility.getVolumeUnitForSymbol(VOLUME_UNIT_Quart_US_LIQ));
         assertEquals(Units.CUBIC_METRE.multiply(0.00001478676), UnitConversionUtility.getVolumeUnitForSymbol(VOLUME_UNIT_Tablespoon));
         assertEquals(Units.CUBIC_METRE.multiply(0.000004928922), UnitConversionUtility.getVolumeUnitForSymbol(VOLUME_UNIT_Teaspoon));
         assertEquals(Units.CUBIC_METRE.multiply(2.831685), UnitConversionUtility.getVolumeUnitForSymbol(VOLUME_UNIT_TON_REGISTER));
         assertEquals(Units.CUBIC_METRE.multiply(0.7645549), UnitConversionUtility.getVolumeUnitForSymbol(VOLUME_UNIT_CY));
         assertEquals(Units.CUBIC_METRE.multiply(1000), UnitConversionUtility.getVolumeUnitForSymbol(VOLUME_UNIT_ML));
         assertEquals(Units.CUBIC_METRE.multiply(0.001), UnitConversionUtility.getVolumeUnitForSymbol(VOLUME_UNIT_D3));
     }
//    @Test
//     void testGetVolumeUnitForSymbol() {
//        assertEquals(Units.LITRE, UnitConversionUtility.getVolumeUnitForSymbol(VOLUME_UNIT_LITRE));
//        assertEquals(Units.LITRE.multiply(0.001), UnitConversionUtility.getVolumeUnitForSymbol(VOLUME_UNIT_ML));
//        assertEquals(Units.LITRE.multiply(1000), UnitConversionUtility.getVolumeUnitForSymbol(VOLUME_UNIT_KL));
//        assertEquals(Units.LITRE.multiply(0.000001), UnitConversionUtility.getVolumeUnitForSymbol(VOLUME_UNIT_UL));
//        assertEquals(Units.LITRE.multiply(0.000000001), UnitConversionUtility.getVolumeUnitForSymbol(VOLUME_UNIT_NL));
//        assertEquals(Units.LITRE.multiply(0.000001), UnitConversionUtility.getVolumeUnitForSymbol(VOLUME_UNIT_MICROL));
//        assertEquals(Units.LITRE.multiply(4.92892E-6), UnitConversionUtility.getVolumeUnitForSymbol(TEASPOON));
//        assertEquals(Units.LITRE.multiply(0.014787), UnitConversionUtility.getVolumeUnitForSymbol(TABLESPOON));
//        assertEquals(Units.LITRE.multiply(0.236588), UnitConversionUtility.getVolumeUnitForSymbol(CUP));
//        assertEquals(Units.LITRE.multiply(0.00378541), UnitConversionUtility.getVolumeUnitForSymbol(FLUID_OUNCE));
//        assertEquals(Units.LITRE.multiply(0.473176), UnitConversionUtility.getVolumeUnitForSymbol(PINT));
//        assertEquals(Units.LITRE.multiply(0.946353), UnitConversionUtility.getVolumeUnitForSymbol(QUART));
//        assertEquals(Units.LITRE.multiply(3.78541), UnitConversionUtility.getVolumeUnitForSymbol(GALLON));
//        assertEquals(Units.LITRE.multiply(0.568261), UnitConversionUtility.getVolumeUnitForSymbol(PINT_UK));
//        assertEquals(Units.LITRE.multiply(1.13652), UnitConversionUtility.getVolumeUnitForSymbol(QUART_UK));
//        assertEquals(Units.LITRE.multiply(4.54609), UnitConversionUtility.getVolumeUnitForSymbol(GALLON_UK));
//        assertEquals(Units.LITRE.multiply(0.0351951), UnitConversionUtility.getVolumeUnitForSymbol(GILL_UK));
//        assertEquals(Units.LITRE.multiply(1E-6), UnitConversionUtility.getVolumeUnitForSymbol(MICROLITER));
//        assertEquals(Units.LITRE.multiply(0.000001), UnitConversionUtility.getVolumeUnitForSymbol(MICROLITER));
//        assertEquals(Units.LITRE.multiply(1E-9), UnitConversionUtility.getVolumeUnitForSymbol(NANOLITER));
//        assertEquals(Units.LITRE.multiply(1E-12), UnitConversionUtility.getVolumeUnitForSymbol(PICOLITER));
//        assertEquals(Units.LITRE.multiply(0.0000163871), UnitConversionUtility.getVolumeUnitForSymbol(CUBIC_INCH));
//        assertEquals(Units.LITRE.multiply(0.0283168), UnitConversionUtility.getVolumeUnitForSymbol(CUBIC_FOOT));
//        assertEquals(Units.LITRE.multiply(0.764555), UnitConversionUtility.getVolumeUnitForSymbol(CUBIC_YARD));
//        assertEquals(Units.LITRE.multiply(1E-6), UnitConversionUtility.getVolumeUnitForSymbol(MICROLITER));
//        assertEquals(Units.LITRE.multiply(0.000001), UnitConversionUtility.getVolumeUnitForSymbol(MICROLITER));
//        assertEquals(Units.LITRE.multiply(1E-9), UnitConversionUtility.getVolumeUnitForSymbol(NANOLITER));
//        assertEquals(Units.LITRE.multiply(1E-12), UnitConversionUtility.getVolumeUnitForSymbol(PICOLITER));
//    }

    @Test
     void testGetLengthUnitForSymbol_Invalid() {
        IllegalArgumentException thrown = assertThrows(IllegalArgumentException.class, () -> {
            UnitConversionUtility.getLengthUnitForSymbol("invalid");
        });

        assertEquals(DAO_UNKNOWN_UNIT + "invalid", thrown.getMessage());
    }

    @Test
     void testGetWeightUnitForSymbol_Invalid() {
        IllegalArgumentException thrown = assertThrows(IllegalArgumentException.class, () -> {
            UnitConversionUtility.getWeightUnitForSymbol("invalid");
        });

        assertEquals(DAO_UNKNOWN_UNIT + "invalid", thrown.getMessage());
    }

    @Test
     void testGetVolumeUnitForSymbol_Invalid() {
        IllegalArgumentException thrown = assertThrows(IllegalArgumentException.class, () -> {
            UnitConversionUtility.getVolumeUnitForSymbol("invalid");
        });

        assertEquals(DAO_UNKNOWN_UNIT + "invalid", thrown.getMessage());
    }
}
