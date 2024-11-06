package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.shipment.services.entity.CarrierBooking;
import com.dpw.runner.shipment.services.utils.ExcludeTenantFilter;
import com.dpw.runner.shipment.services.utils.Generated;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

@Repository
@Generated
public interface ICarrierBookingRepository extends MultiTenancyRepository<CarrierBooking> {

    List<CarrierBooking> findAll();

    Page<CarrierBooking> findAll(Specification<CarrierBooking> spec, Pageable pageable);

    default Optional<CarrierBooking> findByGuid(UUID id) {
        Specification<CarrierBooking> spec = (root, criteriaQuery, criteriaBuilder) -> criteriaBuilder.equal(root.get("guid"), id);
        return findOne(spec);
    }

    @Query(value = "SELECT id FROM carrier_booking WHERE bol = ?1 and tenant_id = ?2", nativeQuery = true)
    List<Long> findByBol (String bol, Integer tenantId);

    @Query(value = "SELECT id FROM carrier_booking WHERE forwarder_ref_number = ?1 and tenant_id = ?2", nativeQuery = true)
    List<Long> findByForwarderRefNumber(String forwarderRefNumber, Integer tenantId);

    @ExcludeTenantFilter
    @Query(value = "SELECT MAX(id) FROM carrier_booking", nativeQuery = true)
    Optional<Long> findMaxId();

    @Query(value = "SELECT EXISTS (SELECT 1 FROM carrier_booking WHERE booking_id = ?1)", nativeQuery = true)
    boolean existsByIntraBookingId(String intraBookingId);

}
