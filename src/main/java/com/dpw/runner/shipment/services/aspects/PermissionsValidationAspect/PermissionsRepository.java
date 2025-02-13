package com.dpw.runner.shipment.services.aspects.PermissionsValidationAspect;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.repository.NoRepositoryBean;

import java.util.Optional;

@NoRepositoryBean
public interface PermissionsRepository<T> extends JpaRepository<T, Long>, JpaSpecificationExecutor<T> {

    Optional<T> findOneById(Long id);
}
