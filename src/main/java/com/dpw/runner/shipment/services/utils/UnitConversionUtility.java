package com.dpw.runner.shipment.services.utils;

import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import io.micrometer.core.instrument.Metrics;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import tec.units.ri.quantity.Quantities;
import tec.units.ri.unit.MetricPrefix;
import tec.units.ri.unit.Units;

import javax.measure.Quantity;
import javax.measure.Unit;
import javax.measure.UnitConverter;
import java.math.BigDecimal;

@Slf4j
@Component
public class UnitConversionUtility {
    public static Number convertUnit(String type, BigDecimal value, String fromUnit, String toUnit) throws Exception {
        String responseMsg;
        try {
            Unit<?> sourceUnit = getUnitType(type, fromUnit);
            Unit<?> targetUnit = getUnitType(type, toUnit);
            UnitConverter converter = sourceUnit.getConverterToAny(targetUnit);
            return converter.convert(value);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_CALCULATION_ERROR;
            log.error(responseMsg, e);
            throw new Exception(e);
        }
    }

    private static Unit<?> getUnitType(String type, String unitSymbol) {
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

    private static Unit<?> getLengthUnitForSymbol(String unitSymbol) {
        switch (unitSymbol) {
            case Constants.METRE:
                return Units.METRE;
            case Constants.CENTI:
                return MetricPrefix.CENTI(Units.METRE);
            case Constants.DECI:
                return MetricPrefix.DECI(Units.METRE);
            default:
                throw new IllegalArgumentException(DaoConstants.DAO_UNKNOWN_UNIT + unitSymbol);
        }
    }

    private static Unit<?> getWeightUnitForSymbol(String unitSymbol) {
        switch (unitSymbol) {
            case Constants.WEIGHT_UNIT_KG:
                return MetricPrefix.KILO(Units.GRAM);
            case Constants.WEIGHT_UNIT_GRAM:
                return Units.GRAM;
            case Constants.METRIC_TON:
                return MetricPrefix.MEGA(Units.GRAM);
            default:
                throw new IllegalArgumentException(DaoConstants.DAO_UNKNOWN_UNIT + unitSymbol);
        }
    }

    private static Unit<?> getVolumeUnitForSymbol(String unitSymbol) {
        switch (unitSymbol) {
            case Constants.VOLUME_UNIT_M3:
                return Units.CUBIC_METRE;
            case Constants.VOLUME_UNIT_LITRE:
                return Units.LITRE;
            default:
                throw new IllegalArgumentException(DaoConstants.DAO_UNKNOWN_UNIT + unitSymbol);
        }
    }
}
