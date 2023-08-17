package com.dpw.runner.shipment.services.dao.interfaces;

import com.dpw.runner.shipment.services.entity.ProductSequenceConfig;

import java.util.List;

public interface IProductSequenceConfigDao {

    ProductSequenceConfig save(ProductSequenceConfig productSequenceConfig);
    List<ProductSequenceConfig> saveAll(List<ProductSequenceConfig> productSequenceConfigList);
    List<ProductSequenceConfig> saveEntityFromSettings(List<ProductSequenceConfig> productSequenceConfigList, Long shipmentSettingsId);
    List<ProductSequenceConfig> updateEntityFromSettings(List<ProductSequenceConfig> productSequenceConfigList, Long shipmentSettingsId) throws Exception;

}
