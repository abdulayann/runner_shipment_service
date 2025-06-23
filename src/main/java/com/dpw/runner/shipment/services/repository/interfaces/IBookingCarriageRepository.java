package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.shipment.services.entity.BookingCarriage;
import com.dpw.runner.shipment.services.utils.Generated;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

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
}
