package com.dpw.runner.shipment.services.dao.impl;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;
import static com.dpw.runner.shipment.services.utils.CommonUtils.constructListCommonRequest;

import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dao.interfaces.IProductSequenceConfigDao;
import com.dpw.runner.shipment.services.entity.ProductSequenceConfig;
import com.dpw.runner.shipment.services.entity.enums.LoggerEvent;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.repository.interfaces.IProductSequenceConfigRepository;
import com.nimbusds.jose.util.Pair;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;
import java.util.function.Function;
import java.util.stream.Collectors;
import jakarta.persistence.EntityManager;
import jakarta.persistence.LockModeType;
import jakarta.persistence.PersistenceContext;
import jakarta.persistence.TypedQuery;
import jakarta.persistence.criteria.CriteriaBuilder;
import jakarta.persistence.criteria.CriteriaQuery;
import jakarta.persistence.criteria.Order;
import jakarta.persistence.criteria.Predicate;
import jakarta.persistence.criteria.Root;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;

@Repository
@Slf4j
public class ProductSequenceConfigDao implements IProductSequenceConfigDao {

    @Autowired
    private IProductSequenceConfigRepository productSequenceConfigRepository;
    @PersistenceContext
    EntityManager entityManager;

    @Override
    public ProductSequenceConfig save(ProductSequenceConfig productSequenceConfig) {
        long start = System.currentTimeMillis();
        productSequenceConfig =  productSequenceConfigRepository.save(productSequenceConfig);
        log.info("CR-ID {} || {} for event {} | Time: {} ms", LoggerHelper.getRequestIdFromMDC(),  LoggerEvent.TIME_TAKEN, LoggerEvent.PRODUCT_SEQ_SAVE, System.currentTimeMillis() - start);
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
        log.info("CR-ID {} || {} for event {} | Time: {} ms", LoggerHelper.getRequestIdFromMDC(),  LoggerEvent.TIME_TAKEN, LoggerEvent.PRODUCT_SEQ_SAVE_ALL, System.currentTimeMillis() - start);
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
            // LATER- Handle Transactions here
            ListCommonRequest listCommonRequest = constructListCommonRequest("shipmentSettingsId", shipmentSettingsId, "=");
            Pair<Specification<ProductSequenceConfig>, Pageable> pair = fetchData(listCommonRequest, ProductSequenceConfig.class);
            Page<ProductSequenceConfig> productSequenceConfigs = findAll(pair.getLeft(), pair.getRight());
            Map<Long, ProductSequenceConfig> hashMap = productSequenceConfigs.stream()
                    .collect(Collectors.toMap(ProductSequenceConfig::getId, Function.identity()));
            List<ProductSequenceConfig> productSequenceConfigRequestList = new ArrayList<>();
            if (productSequenceConfigList != null && !productSequenceConfigList.isEmpty()) {
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
            if(oldProductSequenceConfig != null && !oldProductSequenceConfig.isEmpty())
                hashMap = oldProductSequenceConfig.stream().collect(Collectors.toMap(ProductSequenceConfig::getGuid, Function.identity()));
            List<ProductSequenceConfig> productSequenceConfigRequestList = new ArrayList<>();
            if (productSequenceConfigList != null && !productSequenceConfigList.isEmpty()) {
                for (ProductSequenceConfig request : productSequenceConfigList) {
                    UUID guid = request.getGuid();
                    if(hashMap.containsKey(guid)) {
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
        // Step 1: Get a CriteriaBuilder to construct a type-safe query
        CriteriaBuilder cb = entityManager.getCriteriaBuilder();

        // Step 2: Create a CriteriaQuery for ProductSequenceConfig
        CriteriaQuery<ProductSequenceConfig> cq = cb.createQuery(ProductSequenceConfig.class);

        // Step 3: Define the root entity (FROM clause)
        Root<ProductSequenceConfig> root = cq.from(ProductSequenceConfig.class);

        // Step 4: Convert Specification into a WHERE clause (dynamic filtering)
        Predicate predicate = spec.toPredicate(root, cq, cb);
        if (predicate != null) {
            cq.where(predicate);
        }

        // Step 5: Disable DISTINCT because PostgresSQL does not allow DISTINCT with FOR UPDATE
        cq.distinct(false);

        // Step 6: Apply sorting from Pageable (ORDER BY)
        List<Order> orders = new ArrayList<>();
        pageable.getSort().forEach(order ->
                orders.add(order.isAscending() ?
                        cb.asc(root.get(order.getProperty())) : // Ascending sort
                        cb.desc(root.get(order.getProperty()))  // Descending sort
                ));
        if (!orders.isEmpty()) {
            cq.orderBy(orders);
        }

        // Step 7: Build the executable query
        TypedQuery<ProductSequenceConfig> query = entityManager.createQuery(cq);

        // Step 8: Acquire a PESSIMISTIC_WRITE lock (FOR UPDATE)
        // This ensures no other transaction can modify the row until our transaction commits
        query.setLockMode(LockModeType.PESSIMISTIC_WRITE);

        // Step 9: Set lock timeout (milliseconds)
        // If the row is already locked by another transaction, wait up to 2 seconds
        query.setHint("jakarta.persistence.lock.timeout", 2000);

        // Step 10: Apply pagination (offset + page size)
        query.setFirstResult((int) pageable.getOffset()); // skip rows
        query.setMaxResults(pageable.getPageSize());     // limit number of rows

        // Step 11: Execute query and return the first row or null
        // Only the first matching row is locked
        List<ProductSequenceConfig> results = query.getResultList();
        return results.isEmpty() ? null : results.get(0);

    }
}
