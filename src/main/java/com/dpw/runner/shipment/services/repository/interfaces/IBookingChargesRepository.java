package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.shipment.services.entity.BookingCharges;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.utils.Generated;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;
import java.util.Set;

@Repository @Generated
public interface IBookingChargesRepository extends MultiTenancyRepository<BookingCharges> {
    List<BookingCharges> findAll();

    Page<BookingCharges> findAll(Specification<BookingCharges> spec, Pageable pageable);

    default Optional<BookingCharges> findById(Long id) {
        Specification<BookingCharges> spec = (root, criteriaQuery, criteriaBuilder) -> criteriaBuilder.equal(root.get("id"), id);
        return findOne(spec);
    }
    @Modifying
    @Query(value = "UPDATE booking_charges SET is_deleted = true WHERE id NOT IN (?1) and booking_id = ?2", nativeQuery = true)
    void deleteAdditionalChargesByCustomerBookingId(List<Long> bookingChargesIds, Long bookingId);

    @Modifying
    @Query(value = "UPDATE booking_charges SET is_deleted = false WHERE id IN (?1) and booking_id = ?2", nativeQuery = true)
    void revertSoftDeleteByChargesAndBookingId(List<Long> bookingChargesIds, Long bookingId);

    @Query(value = "SELECT c.id FROM booking_charges c WHERE c.booking_id = ?1", nativeQuery = true)
    Set<Long> findByBookingId(Long bookingId);
}
