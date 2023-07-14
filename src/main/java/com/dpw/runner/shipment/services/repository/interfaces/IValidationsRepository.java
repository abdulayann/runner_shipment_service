package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.shipment.services.entity.Validations;
import com.dpw.runner.shipment.services.entity.enums.LifecycleHooks;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

@Repository
public interface IValidationsRepository extends MultiTenancyRepository<Validations> {

    Optional<List<Validations>> findByLifecycleHookAndEntity(LifecycleHooks lifecycleHook, String entity);
}
