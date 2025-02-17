package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dao.interfaces.IProductSequenceConfigDao;
import com.dpw.runner.shipment.services.entity.ProductSequenceConfig;
import com.dpw.runner.shipment.services.entity.enums.LoggerEvent;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.repository.interfaces.IProductSequenceConfigRepository;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;

import javax.persistence.EntityManager;
import javax.persistence.LockModeType;
import javax.persistence.PersistenceContext;
import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;
import static com.dpw.runner.shipment.services.utils.CommonUtils.constructListCommonRequest;

@Repository
@Slf4j
public class ProductSequenceConfigDao implements IProductSequenceConfigDao {

    @PersistenceContext
    EntityManager entityManager;
    @Autowired
    private IProductSequenceConfigRepository productSequenceConfigRepository;

    @Override
    public ProductSequenceConfig save(ProductSequenceConfig productSequenceConfig) {
        long start = System.currentTimeMillis();
        productSequenceConfig = productSequenceConfigRepository.save(productSequenceConfig);
        log.info("CR-ID {} || {} for event {} | Time: {} ms", LoggerHelper.getRequestIdFromMDC(), LoggerEvent.TIME_TAKEN, LoggerEvent.PRODUCT_SEQ_SAVE, System.currentTimeMillis() - start);
        return productSequenceConfig;
    }

    @Override
    public Optional<ProductSequenceConfig> findById(Long id) {
        return productSequenceConfigRepository.findById(id);
    }

    @Override
    public List<ProductSequenceConfig> saveAll(List<ProductSequenceConfig> productSequenceConfigList) {
        long start = System.currentTimeMillis();
        productSequenceConfigList = productSequenceConfigRepository.saveAll(productSequenceConfigList);
        log.info("CR-ID {} || {} for event {} | Time: {} ms", LoggerHelper.getRequestIdFromMDC(), LoggerEvent.TIME_TAKEN, LoggerEvent.PRODUCT_SEQ_SAVE_ALL, System.currentTimeMillis() - start);
        return productSequenceConfigList;
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

            res.add(req);
        }

        return saveAll(res);
    }

    @Override
    public Page<ProductSequenceConfig> findAll(Specification<ProductSequenceConfig> spec, Pageable pageable) {
        return productSequenceConfigRepository.findAll(spec, pageable);
    }

    @Override
    public List<ProductSequenceConfig> updateEntityFromSettings(List<ProductSequenceConfig> productSequenceConfigList, Long shipmentSettingsId) throws RunnerException {
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
            throw new RunnerException(e.getMessage());
        }
    }

    @Override
    public List<ProductSequenceConfig> updateEntityFromV1Settings(List<ProductSequenceConfig> productSequenceConfigList, Long shipmentSettingsId, List<ProductSequenceConfig> oldProductSequenceConfig) throws RunnerException {
        String responseMsg;
        List<ProductSequenceConfig> responseProductSequenceConfig = new ArrayList<>();
        try {
            Map<UUID, ProductSequenceConfig> hashMap = new HashMap<>();
            if (oldProductSequenceConfig != null && oldProductSequenceConfig.size() > 0)
                hashMap = oldProductSequenceConfig.stream().collect(Collectors.toMap(ProductSequenceConfig::getGuid, Function.identity()));
            List<ProductSequenceConfig> productSequenceConfigRequestList = new ArrayList<>();
            if (productSequenceConfigList != null && productSequenceConfigList.size() != 0) {
                for (ProductSequenceConfig request : productSequenceConfigList) {
                    UUID guid = request.getGuid();
                    if (hashMap.containsKey(guid)) {
                        request.setId(hashMap.get(guid).getId());
                        hashMap.remove(guid);
                    }
                    productSequenceConfigRequestList.add(request);
                }
                responseProductSequenceConfig = saveEntityFromSettings(productSequenceConfigRequestList, shipmentSettingsId);
            }
            deleteProductSequenceConfigByUUID(hashMap);
            return responseProductSequenceConfig;
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_FAILED_ENTITY_UPDATE;
            log.error(responseMsg, e);
            throw new RunnerException(e.getMessage());
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

    private void deleteProductSequenceConfigByUUID(Map<UUID, ProductSequenceConfig> hashMap) {
        String responseMsg;
        try {
            hashMap.values().forEach(this::delete);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_DELETE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
    }

    @Override
    public ProductSequenceConfig findAndLock(Specification<ProductSequenceConfig> spec, Pageable pageable) {
        Page<ProductSequenceConfig> page = findAll(spec, pageable);
        ProductSequenceConfig result = null;
        if (!page.isEmpty()) {
            // Acquire lock on this result row
            result = page.getContent().get(0);
            entityManager.lock(result, LockModeType.PESSIMISTIC_WRITE, Map.ofEntries(
                    Map.entry("javax.persistence.lock.timeout", 2000))
            );
        }
        return result;
    }

}
