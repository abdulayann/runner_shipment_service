package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import tec.units.ri.unit.MetricPrefix;
import tec.units.ri.unit.Units;

import javax.measure.Unit;
import javax.measure.UnitConverter;
import java.math.BigDecimal;

@Service
@Slf4j
public class UnitConversionService {
    public Number convertUnit(String type, BigDecimal value, String fromUnit, String toUnit) throws Exception {
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

    private Unit<?> getUnitType(String type, String unitSymbol) {
        switch (type) {
            case Constants.MASS:
                return getWeightUnitForSymbol(unitSymbol);
            case Constants.VOLUME:
                return getVolumeUnitForSymbol(unitSymbol);
            default:
                throw new IllegalArgumentException(DaoConstants.DAO_UNKNOWN_UNIT + type);
        }
    }

    private Unit<?> getWeightUnitForSymbol(String unitSymbol) {
        switch (unitSymbol) {
            case Constants.WEIGHT_UNIT_KG:
                return MetricPrefix.KILO(Units.GRAM);
            case Constants.WEIGHT_UNIT_GRAM:
                return Units.GRAM;
            default:
                throw new IllegalArgumentException(DaoConstants.DAO_UNKNOWN_UNIT + unitSymbol);
        }
    }

    private Unit<?> getVolumeUnitForSymbol(String unitSymbol) {
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
