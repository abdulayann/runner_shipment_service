package com.dpw.runner.shipment.services.dao.interfaces;

import com.dpw.runner.shipment.services.entity.Validations;
import com.dpw.runner.shipment.services.entity.enums.LifecycleHooks;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.util.List;
import java.util.Optional;

public interface IValidationsDao {
    Validations save(Validations views);

    Page<Validations> findAll(Specification<Validations> spec, Pageable pageable);

    Optional<Validations> findById(Long id);

    void delete(Validations views);

    List<Validations> findAll();

    Optional<List<Validations>> findByLifecycleHookAndEntity(LifecycleHooks lifecycleHook, String entity);
}
