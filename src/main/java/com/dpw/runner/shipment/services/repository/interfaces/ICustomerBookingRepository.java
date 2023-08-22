package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.shipment.services.entity.CustomerBooking;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

@Repository
public interface ICustomerBookingRepository extends MultiTenancyRepository<CustomerBooking> {
    List<CustomerBooking> findAll();

    Page<CustomerBooking> findAll(Specification<CustomerBooking> spec, Pageable pageable);

    default Optional<CustomerBooking> findById(Long id) {
        Specification<CustomerBooking> spec = (root, criteriaQuery, criteriaBuilder) -> criteriaBuilder.equal(root.get("id"), id);
        return findOne(spec);
    }
}
