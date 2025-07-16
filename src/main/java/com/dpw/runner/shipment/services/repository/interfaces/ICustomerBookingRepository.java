package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.shipment.services.entity.CustomerBooking;
import com.dpw.runner.shipment.services.utils.Generated;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

@Repository @Generated
public interface ICustomerBookingRepository extends MultiTenancyRepository<CustomerBooking> {
    List<CustomerBooking> findAll();

    Page<CustomerBooking> findAll(Specification<CustomerBooking> spec, Pageable pageable);

    default Optional<CustomerBooking> findById(Long id) {
        Specification<CustomerBooking> spec = (root, criteriaQuery, criteriaBuilder) -> criteriaBuilder.equal(root.get("id"), id);
        return findOne(spec);
    }

    default Optional<CustomerBooking> findByGuid(UUID id) {
        Specification<CustomerBooking> spec = (root, criteriaQuery, criteriaBuilder) -> criteriaBuilder.equal(root.get("guid"), id);
        return findOne(spec);
    }

    Optional<CustomerBooking> findByBookingNumber(String bookingNumber);
    @Modifying
    @Query(value = "Update customer_booking set is_platform_booking_created = ?2 Where id = ?1", nativeQuery = true)
    int updateIsPlatformBookingCreated(Long id, Boolean isPlatformBookingCreated);

    @Modifying
    @Query(value = "Update customer_booking set is_bill_created = ?2 Where id = ?1", nativeQuery = true)
    int updateBillingStatus(Long id, Boolean isBillCreated);

    Optional<CustomerBooking> findByOrderManagementId(String orderId);

    @Query(value = "SELECT * from customer_booking where booking_number = ?1", nativeQuery = true)
    Optional<CustomerBooking> findByBookingNumberQuery(String bookingNumber);

    Optional<CustomerBooking> findByShipmentReferenceNumber(String shipmentReferenceNumber);

    @Query(value = "SELECT cb.id from customer_booking sb where sb.tenant_id = ?1", nativeQuery = true)
    List<Long> findCustomerBookingIdsByTenantId(Integer tenantId);
}
