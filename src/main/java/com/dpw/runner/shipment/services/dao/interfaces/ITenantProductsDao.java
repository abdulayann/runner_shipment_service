package com.dpw.runner.shipment.services.dao.interfaces;

import com.dpw.runner.shipment.services.entity.TenantProducts;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.util.List;

public interface ITenantProductsDao {

    TenantProducts save(TenantProducts tenantProducts);

    List<TenantProducts> saveAll(List<TenantProducts> tenantProductsList);

    Page<TenantProducts> findAll(Specification<TenantProducts> spec, Pageable pageable);

    List<TenantProducts> saveEntityFromSettings(List<TenantProducts> tenantProductsList, Long shipmentSettingsId);

    List<TenantProducts> updateEntityFromSettings(List<TenantProducts> tenantProductsList, Long shipmentSettingsId) throws RunnerException;

    List<TenantProducts> updateEntityFromV1Settings(List<TenantProducts> tenantProductsList, Long shipmentSettingsId, List<TenantProducts> oldTenantProducts) throws RunnerException;

}
