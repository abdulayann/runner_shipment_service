package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.shipment.services.entity.ShippingInstruction;
import com.dpw.runner.shipment.services.projection.CarrierBookingInfoProjection;
import org.springframework.data.jpa.repository.Query;


public interface IShippingInstructionRepository extends MultiTenancyRepository<ShippingInstruction> {
    @Query(value = "select status as bookingStatus, booking_no as bookingNumber, entity_id as entityId " +
            "from carrier_booking where id = ?1 and is_deleted is false;", nativeQuery = true)
    CarrierBookingInfoProjection findCarrierBookingInfoById(Long id);
}
