package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.shipment.services.entity.CarrierBooking;
import org.springframework.data.jpa.repository.Query;

public interface ICarrierBookingRepository extends MultiTenancyRepository<CarrierBooking> {
    @Query(value = "SELECT count(*) from carrier_booking where tenant_id = :tenantId and is_deleted in(true,false)", nativeQuery = true)
    Long getTotalCarrierBookings(Long tenantId);

    @Query(value = "SELECT * FROM carrier_booking where booking_no = :bookingNo and is_deleted = false", nativeQuery = true)
    CarrierBooking findByBookingNo(String bookingNo);
}
