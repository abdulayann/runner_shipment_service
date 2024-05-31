package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.entity.CarrierDetails;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.shipment.services.utils.Generated;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

@Repository @Generated
public interface ICarrierRepository extends MultiTenancyRepository<CarrierDetails> {
    List<CarrierDetails> findAll();
    Page<CarrierDetails> findAll(Specification<CarrierDetails> spec, Pageable pageable);
    default Optional<CarrierDetails> findById(Long id) {
        Specification<CarrierDetails> spec = (root, criteriaQuery, criteriaBuilder) -> criteriaBuilder.equal(root.get("id"), id);
        return findOne(spec);
    }
}
