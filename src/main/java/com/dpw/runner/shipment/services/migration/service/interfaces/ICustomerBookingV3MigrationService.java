package com.dpw.runner.shipment.services.migration.service.interfaces;

import com.dpw.runner.shipment.services.entity.CustomerBooking;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;

import java.io.IOException;
import java.math.BigDecimal;
import java.util.Map;

public interface ICustomerBookingV3MigrationService {
    CustomerBooking migrateBookingV2ToV3(Long bookingId, Map<String, BigDecimal> codeTeuMap, Integer weightDecimal, Integer volumeDecimal) throws RunnerException, IOException;
    CustomerBooking migrateBookingV3ToV2(Long bookingId, Integer weightDecimal, Integer volumeDecimal) throws RunnerException;

    Map<String, Object> migrateBookingV2ToV3ForTenant(Integer tenantId, Map<String, BigDecimal> codeTeuMap, Integer weightDecimal, Integer volumeDecimal);
    Map<String, Integer> migrateBookingV3ToV2ForTenant(Integer tenantId, Integer weightDecimal, Integer volumeDecimal);
}
