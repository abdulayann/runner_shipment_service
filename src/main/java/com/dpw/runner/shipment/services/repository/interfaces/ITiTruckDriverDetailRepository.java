package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.shipment.services.entity.TiTruckDriverDetails;
import com.dpw.runner.shipment.services.utils.Generated;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

@Repository
@Generated
public interface ITiTruckDriverDetailRepository extends MultiTenancyRepository<TiTruckDriverDetails> {
    List<TiTruckDriverDetails> findAll();
    Page<TiTruckDriverDetails> findAll(Specification<TiTruckDriverDetails> spec, Pageable pageable);
    default Optional<TiTruckDriverDetails> findById(Long id) {
        Specification<TiTruckDriverDetails> spec = (root, criteriaQuery, criteriaBuilder) -> criteriaBuilder.equal(root.get("id"), id);
        return findOne(spec);
    }

    List<TiTruckDriverDetails> findByIdIn(List<Long> ids);
}
