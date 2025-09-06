package com.dpw.runner.shipment.services.migration.service.interfaces;

import com.dpw.runner.shipment.services.entity.CustomerBooking;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;

import java.math.BigDecimal;
import java.util.Map;

public interface ICustomerBookingV3MigrationService {
    CustomerBooking migrateBookingV2ToV3(Long bookingId, Map<String, BigDecimal> codeTeuMap, Integer weightDecimal, Integer volumeDecimal) throws RunnerException;
    CustomerBooking migrateBookingV3ToV2(Long bookingId, Integer weightDecimal, Integer volumeDecimal) throws RunnerException;

    Map<String, Integer> migrateBookingV2ToV3ForTenant(Integer tenantId, Map<String, BigDecimal> codeTeuMap, Integer weightDecimal, Integer volumeDecimal);
    Map<String, Integer> migrateBookingV3ToV2ForTenant(Integer tenantId, Integer weightDecimal, Integer volumeDecimal);
}
