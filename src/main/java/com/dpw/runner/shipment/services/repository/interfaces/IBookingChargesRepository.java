package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.shipment.services.entity.BookingCharges;
import com.dpw.runner.shipment.services.utils.Generated;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

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
    void deleteAdditionalPackingByCustomerBookingId(List<Long> bookingChargesIds, Long bookingId);

    @Modifying
    @Query(value = "UPDATE booking_charges SET is_deleted = false WHERE id IN (?1) and booking_id = ?2", nativeQuery = true)
    void revertSoftDeleteByPackingIdsAndBookingId(List<Long> bookingChargesIds, Long bookingId);
}
