package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.shipment.services.entity.Validations;
import com.dpw.runner.shipment.services.entity.enums.LifecycleHooks;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

@Repository
public interface IValidationsRepository extends MultiTenancyRepository<Validations> {

    @Query(value = "select * from validations u where u.lifecycle_hook = ?1 and u.entity = ?2", nativeQuery = true)
    Optional<List<Validations>> findByLifecycleHookAndEntity(String lifecycleHook, String entity);

    default Optional<Validations> findById(Long id) {
        Specification<Validations> spec = (root, criteriaQuery, criteriaBuilder) -> criteriaBuilder.equal(root.get("id"), id);
        return findOne(spec);
    }
    List<Validations> findAll();
}
