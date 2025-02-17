package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.shipment.services.entity.AdditionalDetails;
import com.dpw.runner.shipment.services.utils.Generated;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.data.jpa.repository.Query;

import java.util.List;
import java.util.Optional;

@Generated
public interface IAdditionalDetailRepository extends MultiTenancyRepository<AdditionalDetails> {
    List<AdditionalDetails> findAll();

    Page<AdditionalDetails> findAll(Specification<AdditionalDetails> spec, Pageable pageable);

    default Optional<AdditionalDetails> findById(Long id) {
        Specification<AdditionalDetails> spec = (root, criteriaQuery, criteriaBuilder) -> criteriaBuilder.equal(root.get("id"), id);
        return findOne(spec);
    }

    List<AdditionalDetails> findByIdIn(List<Long> ids);
}