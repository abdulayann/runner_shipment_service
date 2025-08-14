package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.shipment.services.entity.BookingCarriage;
import com.dpw.runner.shipment.services.utils.Generated;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.Optional;
@Generated
public interface IBookingCarriageRepository extends MultiTenancyRepository<BookingCarriage> {
    List<BookingCarriage> findAll();
    Page<BookingCarriage> findAll(Specification<BookingCarriage> spec, Pageable pageable);
    default Optional<BookingCarriage> findById(Long id) {
        Specification<BookingCarriage> spec = (root, criteriaQuery, criteriaBuilder) -> criteriaBuilder.equal(root.get("id"), id);
        return findOne(spec);
    }
  List<BookingCarriage> findByShipmentId(Long shipmentId);

    @Modifying
    @Transactional
    @Query(value = "UPDATE booking_carriage SET is_deleted = true WHERE id NOT IN (?1) and shipment_id = ?2", nativeQuery = true)
    void deleteAdditionalbookingCarriageByShipmentId(List<Long> bookingCarriageIds, Long shipmentId);

    @Modifying
    @Transactional
    @Query(value = "UPDATE booking_carriage SET is_deleted = false WHERE id IN (?1) and shipment_id = ?2", nativeQuery = true)
    void revertSoftDeleteBybookingCarriageIdsAndShipmentId(List<Long> bookingCarriageIds, Long shipmentId);
}
