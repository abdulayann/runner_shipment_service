package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.shipment.services.entity.ShippingInstruction;
import com.dpw.runner.shipment.services.projection.CarrierBookingInfoProjection;
import com.dpw.runner.shipment.services.projection.ShippingConsoleIdProjection;
import com.dpw.runner.shipment.services.projection.ShippingConsoleNoProjection;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.util.List;


public interface IShippingInstructionRepository extends MultiTenancyRepository<ShippingInstruction> {
    @Query(value = "select status as bookingStatus, booking_no as bookingNumber, entity_id as entityId " +
            "from carrier_booking where id = ?1 and is_deleted is false;", nativeQuery = true)
    CarrierBookingInfoProjection findCarrierBookingInfoById(Long id);

    @Query(value = "SELECT si.id AS id, si.entity_number AS entityId " +
       "FROM shipping_instruction si " +
       "WHERE si.entity_type = :entityType and is_deleted is false " +
       "AND si.entity_number IN (:consols);", nativeQuery = true)
    List<ShippingConsoleNoProjection> findByEntityTypeAndEntityNo(@Param("entityType") String entityType,
                                                                  @Param("consols") List<String> allConsolNumbers);

    @Query(value = "SELECT si.id AS id, cb.entity_number AS entityId " +
            "FROM shipping_instruction si " +
            "JOIN carrier_booking cb ON si.entity_type = 'CARRIER_BOOKING' " +
            "AND si.entity_id = cb.id " +
            "WHERE cb.entity_type = 'CONSOLIDATION' " +
            "AND cb.entity_number IN (?1)", nativeQuery = true)
    List<ShippingConsoleNoProjection> findByCarrierBookingConsolNumbers(List<String> consolNumbers);

    @Query(value = "SELECT si.id AS id, si.entity_id AS entityId " +
            "FROM shipping_instruction si " +
            "WHERE si.entity_type = :entityType and is_deleted is false " +
            "AND si.entity_id IN (:consols);", nativeQuery = true)
    List<ShippingConsoleIdProjection> findByEntityTypeAndEntityId(@Param("entityType") String entityType,
                                                                  @Param("consols") List<Long> allConsolId);

    @Query(value = "SELECT si.id AS id, cb.entity_id AS entityId " +
            "FROM shipping_instruction si " +
            "JOIN carrier_booking cb ON si.entity_type = 'CARRIER_BOOKING' " +
            "AND si.entity_id = cb.id " +
            "WHERE cb.entity_type = 'CONSOLIDATION' " +
            "AND cb.entity_id IN (?1)", nativeQuery = true)
    List<ShippingConsoleIdProjection> findByCarrierBookingConsolId(List<Long> consolNumbers);

    @Query(value = "select status as bookingStatus, booking_no as bookingNumber, entity_id as entityId " +
            "from carrier_booking where entity_number = ?1 and entity_type='CONSOLIDATION' " +
            "and status!='Cancelled' and is_deleted is false;", nativeQuery = true)
    List<CarrierBookingInfoProjection> findByConsolidationNo(String consolNumber);

}
