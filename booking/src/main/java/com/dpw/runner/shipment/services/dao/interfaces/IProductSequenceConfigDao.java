package com.dpw.runner.shipment.services.dao.interfaces;

import com.dpw.runner.shipment.services.entity.ProductSequenceConfig;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.util.List;
import java.util.Optional;

public interface IProductSequenceConfigDao {

    ProductSequenceConfig save(ProductSequenceConfig productSequenceConfig);
    List<ProductSequenceConfig> saveAll(List<ProductSequenceConfig> productSequenceConfigList);
    List<ProductSequenceConfig> saveEntityFromSettings(List<ProductSequenceConfig> productSequenceConfigList, Long shipmentSettingsId);
    List<ProductSequenceConfig> updateEntityFromSettings(List<ProductSequenceConfig> productSequenceConfigList, Long shipmentSettingsId) throws RunnerException;
    List<ProductSequenceConfig> updateEntityFromV1Settings(List<ProductSequenceConfig> productSequenceConfigList, Long shipmentSettingsId, List<ProductSequenceConfig> oldProductSequenceConfig) throws RunnerException;
    Page<ProductSequenceConfig> findAll(Specification<ProductSequenceConfig> spec, Pageable pageable);
    Optional<ProductSequenceConfig> findById(Long id);
    ProductSequenceConfig findAndLock(Specification<ProductSequenceConfig> spec, Pageable pageable);
}
