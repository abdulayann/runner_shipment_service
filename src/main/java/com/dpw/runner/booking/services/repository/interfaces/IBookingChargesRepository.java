package com.dpw.runner.booking.services.repository.interfaces;

import com.dpw.runner.booking.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.booking.services.entity.BookingCharges;
import com.dpw.runner.booking.services.utils.Generated;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
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
}
