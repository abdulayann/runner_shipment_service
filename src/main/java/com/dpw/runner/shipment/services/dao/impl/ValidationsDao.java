package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.dao.interfaces.IValidationsDao;
import com.dpw.runner.shipment.services.entity.Validations;
import com.dpw.runner.shipment.services.entity.enums.LifecycleHooks;
import com.dpw.runner.shipment.services.repository.interfaces.IValidationsRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

@Repository
public class ValidationsDao implements IValidationsDao {

    @Autowired
    private IValidationsRepository validationsRepository;

    @Override
    public Validations save(Validations validation) {
        return validationsRepository.save(validation);
    }

    @Override
    public Page<Validations> findAll(Specification<Validations> spec, Pageable pageable) {
        // LATER: To implement if needed
        return null;
    }

    @Override
    public Optional<Validations> findById(Long id) {
        return validationsRepository.findById(id);
    }

    @Override
    public void delete(Validations validation) {
        validationsRepository.delete(validation);
    }

    @Override
    public List<Validations> findAll() {
        return validationsRepository.findAll();
    }

    @Override
    public Optional<List<Validations>> findByLifecycleHookAndEntity(LifecycleHooks lifecycleHook, String entity) {
        Integer tenantId = UserContext.getUser().getTenantId();
        return validationsRepository.findByLifecycleHookAndEntity(lifecycleHook.name(), entity, tenantId);
    }
}
