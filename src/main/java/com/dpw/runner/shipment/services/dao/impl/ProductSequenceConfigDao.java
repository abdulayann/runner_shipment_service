package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dao.interfaces.IProductSequenceConfigDao;
import com.dpw.runner.shipment.services.entity.ProductSequenceConfig;
import com.dpw.runner.shipment.services.entity.TenantProducts;
import com.dpw.runner.shipment.services.repository.interfaces.IProductSequenceConfigRepository;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.function.Function;
import java.util.stream.Collectors;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;
import static com.dpw.runner.shipment.services.utils.CommonUtils.constructListCommonRequest;

@Repository
@Slf4j
public class ProductSequenceConfigDao implements IProductSequenceConfigDao {

    @Autowired
    private IProductSequenceConfigRepository productSequenceConfigRepository;

    @Override
    public ProductSequenceConfig save(ProductSequenceConfig productSequenceConfig) {
        return productSequenceConfigRepository.save(productSequenceConfig);
    }

    private Optional<ProductSequenceConfig> findById(Long id) {
        return productSequenceConfigRepository.findById(id);
    }

    @Override
    public List<ProductSequenceConfig> saveAll(List<ProductSequenceConfig> productSequenceConfigList) {
        return productSequenceConfigRepository.saveAll(productSequenceConfigList);
    }

    @Override
    public List<ProductSequenceConfig> saveEntityFromSettings(List<ProductSequenceConfig> productSequenceConfigList, Long shipmentSettingsId) {
        List<ProductSequenceConfig> res = new ArrayList<>();
        for (ProductSequenceConfig req : productSequenceConfigList) {
            if (req.getId() != null) {
                long id = req.getId();
                Optional<ProductSequenceConfig> oldEntity = findById(id);
                if (!oldEntity.isPresent()) {
                    log.debug("Product Sequence Config is null for Id {}", req.getId());
                    throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
                }
            }
            req.setShipmentSettingsId(shipmentSettingsId);
            req = save(req);
            res.add(req);
        }
        return res;
    }

    public Page<ProductSequenceConfig> findAll(Specification<ProductSequenceConfig> spec, Pageable pageable) {
        return productSequenceConfigRepository.findAll(spec, pageable);
    }

    @Override
    public List<ProductSequenceConfig> updateEntityFromSettings(List<ProductSequenceConfig> productSequenceConfigList, Long shipmentSettingsId) throws Exception {
        String responseMsg;
        List<ProductSequenceConfig> responseProductSequenceConfig = new ArrayList<>();
        try {
            // TODO- Handle Transactions here
            ListCommonRequest listCommonRequest = constructListCommonRequest("shipmentSettingsId", shipmentSettingsId, "=");
            Pair<Specification<ProductSequenceConfig>, Pageable> pair = fetchData(listCommonRequest, ProductSequenceConfig.class);
            Page<ProductSequenceConfig> productSequenceConfigs = findAll(pair.getLeft(), pair.getRight());
            Map<Long, ProductSequenceConfig> hashMap = productSequenceConfigs.stream()
                    .collect(Collectors.toMap(ProductSequenceConfig::getId, Function.identity()));
            List<ProductSequenceConfig> productSequenceConfigRequestList = new ArrayList<>();
            if (productSequenceConfigList != null && productSequenceConfigList.size() != 0) {
                for (ProductSequenceConfig request : productSequenceConfigList) {
                    Long id = request.getId();
                    if (id != null) {
                        hashMap.remove(id);
                    }
                    productSequenceConfigRequestList.add(request);
                }
                responseProductSequenceConfig = saveEntityFromSettings(productSequenceConfigRequestList, shipmentSettingsId);
            }
            deleteProductSequenceConfig(hashMap);
            return responseProductSequenceConfig;
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_FAILED_ENTITY_UPDATE;
            log.error(responseMsg, e);
            throw new Exception(e);
        }
    }

    private void delete(ProductSequenceConfig tenantProducts) {
        productSequenceConfigRepository.delete(tenantProducts);
    }

    private void deleteProductSequenceConfig(Map<Long, ProductSequenceConfig> hashMap) {
        String responseMsg;
        try {
            hashMap.values().forEach(this::delete);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_DELETE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
    }

}
