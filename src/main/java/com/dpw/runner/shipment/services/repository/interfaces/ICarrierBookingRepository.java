package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.shipment.services.entity.CarrierBooking;
import com.dpw.runner.shipment.services.projection.CarrierBookingInfoProjection;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.data.jpa.repository.Query;

public interface ICarrierBookingRepository extends MultiTenancyRepository<CarrierBooking> {
    @Query(value = "SELECT count(*) from carrier_booking where tenant_id = :tenantId and is_deleted in(true,false)", nativeQuery = true)
    Long getTotalCarrierBookings(Long tenantId);

    @Query(value = "SELECT * FROM carrier_booking where booking_no = :bookingNo and is_deleted = false", nativeQuery = true)
    CarrierBooking findByBookingNo(String bookingNo);

    Page<CarrierBooking> findAll(Specification<CarrierBooking> spec, Pageable pageable);

    @Query(value = "select cb.status as bookingStatus,si.status as siStatus, si.id as siId " +
            "from carrier_booking cb LEFT JOIN shipping_instruction si ON cb.shipping_instruction_id = si.id where cb.id = ?1 and cb.is_deleted = false;", nativeQuery = true)
    CarrierBookingInfoProjection findCarrierBookingInfoById(Long id);

}
