package com.dpw.runner.shipment.services.migration.service.interfaces;

import com.dpw.runner.shipment.services.entity.CustomerBooking;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;

import java.math.BigDecimal;
import java.util.Map;

public interface ICustomerBookingV3MigrationService {
    CustomerBooking migrateBookingV2ToV3(CustomerBooking customerBooking, Map<String, BigDecimal> codeTeuMap) throws RunnerException;
    CustomerBooking mapBookingV2ToV3(CustomerBooking customerBooking, Map<String, BigDecimal> codeTeuMap) throws RunnerException;

    CustomerBooking migrateBookingV3ToV2(CustomerBooking customerBooking, Map<String, BigDecimal> codeTeuMap) throws RunnerException;
    CustomerBooking mapBookingV3ToV2(CustomerBooking customerBooking, Map<String, BigDecimal> codeTeuMap) throws RunnerException;

    Map<String, Integer> migrateBookingV2ToV3ForTenant(Integer tenantId);
    Map<String, Integer> migrateBookingV3ToV2ForTenant(Integer tenantId);
}
