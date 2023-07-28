package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.shipment.services.entity.Validations;
import com.dpw.runner.shipment.services.entity.enums.LifecycleHooks;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

@Repository
public interface IValidationsRepository extends MultiTenancyRepository<Validations> {

    Optional<List<Validations>> findByLifecycleHookAndEntity(LifecycleHooks lifecycleHook, String entity);
    default Optional<Validations> findById(Long id) {
        Specification<Validations> spec = (root, criteriaQuery, criteriaBuilder) -> criteriaBuilder.equal(root.get("id"), id);
        return findOne(spec);
    }
    List<Validations> findAll();
}
