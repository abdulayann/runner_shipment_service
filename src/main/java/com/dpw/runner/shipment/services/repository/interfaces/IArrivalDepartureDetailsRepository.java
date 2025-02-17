package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.shipment.services.entity.ArrivalDepartureDetails;
import com.dpw.runner.shipment.services.utils.Generated;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;


@Repository
@Generated
public interface IArrivalDepartureDetailsRepository extends MultiTenancyRepository<ArrivalDepartureDetails> {
    List<ArrivalDepartureDetails> findAll();

    Page<ArrivalDepartureDetails> findAll(Specification<ArrivalDepartureDetails> spec, Pageable pageable);

    default Optional<ArrivalDepartureDetails> findById(Long id) {
        Specification<ArrivalDepartureDetails> spec = (root, criteriaQuery, criteriaBuilder) -> criteriaBuilder.equal(root.get("id"), id);
        return findOne(spec);
    }
}
