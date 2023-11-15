package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.DocumentService.DocumentService;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.constants.ShipmentSettingsConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.*;
import com.dpw.runner.shipment.services.dto.request.*;
import com.dpw.runner.shipment.services.dto.response.*;
import com.dpw.runner.shipment.services.entity.HblTermsConditionTemplate;
import com.dpw.runner.shipment.services.entity.ProductSequenceConfig;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.entity.TenantProducts;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.repository.interfaces.IShipmentSettingsRepository;
import com.dpw.runner.shipment.services.service.interfaces.IShipmentSettingsService;
import com.dpw.runner.shipment.services.service_bus.AzureServiceBusTopic;
import com.dpw.runner.shipment.services.service_bus.ISBProperties;
import com.dpw.runner.shipment.services.service_bus.SBUtilsImpl;
import com.dpw.runner.shipment.services.syncing.interfaces.IShipmentSettingsSync;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
import org.apache.kafka.common.protocol.types.Field;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.transaction.interceptor.TransactionAspectSupport;

import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;
import static com.dpw.runner.shipment.services.utils.CommonUtils.*;

@Service
@Slf4j
public class ShipmentSettingsService implements IShipmentSettingsService {

    @Autowired
    private IShipmentSettingsDao shipmentSettingsDao;
    @Autowired
    private DocumentService documentService;

    @Autowired
    private IHawbLockSettingsDao hawbLockSettingsDao;

    @Autowired
    private IMawbLockSettingsDao mawbLockSettingsDao;

    @Autowired
    private IHblLockSettingsDao hblLockSettingsDao;

    @Autowired
    private IHblTermsConditionTemplateDao hblTermsConditionTemplateDao;

    @Autowired
    private ITenantProductsDao tenantProductsDao;

    @Autowired
    private IProductSequenceConfigDao productSequenceConfigDao;

    @Autowired
    private JsonHelper jsonHelper;
    @Autowired
    private SBUtilsImpl sbUtils;
    @Autowired
    private ISBProperties isbProperties;
    @Autowired
    private AzureServiceBusTopic azureServiceBusTopic;
    @Autowired
    IShipmentSettingsSync shipmentSettingsSync;
    @Autowired
    IShipmentSettingsRepository shipmentSettingsRepository;
    @Autowired
    ModelMapper modelMapper;

    @Transactional
    public ResponseEntity<?> create(CommonRequestModel commonRequestModel) {
        String responseMsg;
        ShipmentSettingRequest request = null;
        request = (ShipmentSettingRequest) commonRequestModel.getData();
        if(request == null) {
            log.debug("Request is empty for Shipment Settings create with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }
        ShipmentSettingsDetails shipmentSettingsDetails = convertRequestToEntity(request);
        try {
            shipmentSettingsDetails = shipmentSettingsDao.save(shipmentSettingsDetails);

            if(shipmentSettingsDetails.getHblTermsConditionTemplate() != null) {
                shipmentSettingsDetails.setHblTermsConditionTemplate(hblTermsConditionTemplateDao.saveEntityFromSettings(shipmentSettingsDetails.getHblTermsConditionTemplate(), shipmentSettingsDetails.getId(), true));
            }
            if(shipmentSettingsDetails.getHblHawbBackPrintTemplate() != null) {
                shipmentSettingsDetails.setHblHawbBackPrintTemplate(hblTermsConditionTemplateDao.saveEntityFromSettings(shipmentSettingsDetails.getHblHawbBackPrintTemplate(), shipmentSettingsDetails.getId(), false));
            }
            if(shipmentSettingsDetails.getTenantProducts() != null) {
                shipmentSettingsDetails.setTenantProducts(tenantProductsDao.saveEntityFromSettings(shipmentSettingsDetails.getTenantProducts(), shipmentSettingsDetails.getId()));
            }
            if(shipmentSettingsDetails.getProductSequenceConfig() != null) {
                if(shipmentSettingsDetails.getProductSequenceConfig().size() > 0) {
                    for (ProductSequenceConfig productSequenceConfig: shipmentSettingsDetails.getProductSequenceConfig()) {
                        ListCommonRequest listCommonRequest = constructListCommonRequest("productType", productSequenceConfig.getTenantProducts().getProductType(), "=");
                        Pair<Specification<TenantProducts>, Pageable> pair = fetchData(listCommonRequest, TenantProducts.class);
                        Page<TenantProducts> tenantProducts = tenantProductsDao.findAll(pair.getLeft(), pair.getRight());
                        productSequenceConfig.setTenantProducts(tenantProducts.getContent().get(0));
                    }
                }
                shipmentSettingsDetails.setProductSequenceConfig(productSequenceConfigDao.saveEntityFromSettings(shipmentSettingsDetails.getProductSequenceConfig(), shipmentSettingsDetails.getId()));
            }

            try{
                shipmentSettingsSync.sync(shipmentSettingsDetails);
            } catch (Exception e) {
                log.error("Error Syncing Tenant Settings");
            }

            log.info("Shipment Setting Details created successfully for Id {} with Request Id {}", shipmentSettingsDetails.getId(), LoggerHelper.getRequestIdFromMDC());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            TransactionAspectSupport.currentTransactionStatus().setRollbackOnly();
            throw new RuntimeException(e);
        }
        return ResponseHelper.buildSuccessResponse(convertEntityToDto(shipmentSettingsDetails));
    }

    @Transactional
    public ResponseEntity<?> update(CommonRequestModel commonRequestModel) {
        String responseMsg;
        ShipmentSettingRequest request = (ShipmentSettingRequest) commonRequestModel.getData();
        if(request == null) {
            log.error("Request is empty for Shipment Settings update with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }

        if(request.getId() == null) {
            log.error("Request Id is null for Shipment Settings update with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }
        long id = request.getId();
        Optional<ShipmentSettingsDetails> oldEntity = shipmentSettingsDao.findById(id);
        if(!oldEntity.isPresent()) {
            log.debug("Shipment Setting is null for Id {} with Request Id {}", request.getId(), LoggerHelper.getRequestIdFromMDC());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }

        ShipmentSettingsDetails shipmentSettingsDetails = convertRequestToEntity(request);
        shipmentSettingsDetails.setId(oldEntity.get().getId());
        try {
            shipmentSettingsDetails = shipmentSettingsDao.save(shipmentSettingsDetails);
            log.info("Updated the Shipment Setting details for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            TransactionAspectSupport.currentTransactionStatus().setRollbackOnly();
            throw new RuntimeException(e);
        }
        return ResponseHelper.buildSuccessResponse(convertEntityToDto(shipmentSettingsDetails));
    }

    @Transactional
    @Override
    public ResponseEntity<?> completeUpdate(CommonRequestModel commonRequestModel) throws Exception {
        String responseMsg;
        ShipmentSettingRequest request = (ShipmentSettingRequest) commonRequestModel.getData();
        if(request == null) {
            log.error("Request is empty for Shipment Settings update with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }

        if(request.getId() == null && request.getTenantId() == null) {
            log.error("Request Id and Tenant Id is null for Shipment Settings update with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }
        Optional<ShipmentSettingsDetails> oldEntity = null;
        if(request.getTenantId() != null) {
            ListCommonRequest newRequest = new ListCommonRequest();
            newRequest.setPageNo(1);
            newRequest.setPageSize(Integer.MAX_VALUE);
            newRequest.setFilterCriteria(new ArrayList<>());
            Pair<Specification<ShipmentSettingsDetails>, Pageable> tuple = fetchData(newRequest, ShipmentSettingsDetails.class);
            Page<ShipmentSettingsDetails> shipmentSettingsPage = shipmentSettingsDao.list(tuple.getLeft(), tuple.getRight());
            if(shipmentSettingsPage.get().collect(Collectors.toList()) != null && shipmentSettingsPage.get().collect(Collectors.toList()).size() > 0)
                oldEntity = Optional.ofNullable(shipmentSettingsPage.get().collect(Collectors.toList()).get(0));
        }
        else {
            long id = request.getId();
            oldEntity = shipmentSettingsDao.findById(id);
        }
        if(oldEntity == null || !oldEntity.isPresent()) {
            log.debug("Shipment Setting is null for Id {} with Request Id {}", request.getId(), LoggerHelper.getRequestIdFromMDC());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }

        try {
            request.setId(oldEntity.get().getId());
            request.setGuid(oldEntity.get().getGuid());
            if(request.getHawbLockSettings() != null && oldEntity.get().getHawbLockSettings() != null) {
                request.getHawbLockSettings().setId(oldEntity.get().getHawbLockSettings().getId());
                request.getHawbLockSettings().setGuid(oldEntity.get().getHawbLockSettings().getGuid());
            }
            if(request.getMawbLockSettings() != null && oldEntity.get().getMawbLockSettings() != null) {
                request.getMawbLockSettings().setId(oldEntity.get().getMawbLockSettings().getId());
                request.getMawbLockSettings().setGuid(oldEntity.get().getMawbLockSettings().getGuid());
            }
            if(request.getHblLockSettings() != null && oldEntity.get().getHblLockSettings() != null) {
                request.getHblLockSettings().setId(oldEntity.get().getHblLockSettings().getId());
                request.getHblLockSettings().setGuid(oldEntity.get().getHblLockSettings().getGuid());
            }
            ShipmentSettingsDetails shipmentSettingsDetails = convertRequestToEntity(request);

            List<HblTermsConditionTemplateRequest> hblTermsConditionTemplateList = request.getHblTermsConditionTemplate();
            List<HblTermsConditionTemplateRequest> hblHawbBackPrintTemplateList = request.getHblHawbBackPrintTemplate();
            List<TenantProductsRequest> tenantProductsList = request.getTenantProducts();
            List<ProductSequenceConfigRequest> productSequenceConfigList = request.getProductSequenceConfig();

            List<HblTermsConditionTemplate> oldHblTermsConditionTemplateList = oldEntity.get().getHblTermsConditionTemplate();
            List<HblTermsConditionTemplate> oldHblHawbBackPrintTemplateList = oldEntity.get().getHblHawbBackPrintTemplate();
            List<TenantProducts> oldTenantProductsList = oldEntity.get().getTenantProducts();
            List<ProductSequenceConfig> oldProductSequenceConfigList = oldEntity.get().getProductSequenceConfig();

            shipmentSettingsDetails = shipmentSettingsDao.save(shipmentSettingsDetails);

            ShipmentSettingsDetailsResponse response = jsonHelper.convertValue(shipmentSettingsDetails, ShipmentSettingsDetailsResponse.class);
            response.setTenantId(oldEntity.get().getTenantId());
            if(hblTermsConditionTemplateList == null) {
                response.setHblTermsConditionTemplate(convertToDtoList(oldHblTermsConditionTemplateList, HblTermsConditionTemplateResponse.class));
            }
            if(hblHawbBackPrintTemplateList == null) {
                response.setHblHawbBackPrintTemplate(convertToDtoList(oldHblHawbBackPrintTemplateList, HblTermsConditionTemplateResponse.class));
            }
            if(tenantProductsList == null) {
                response.setTenantProducts(convertToDtoList(oldTenantProductsList, TenantProductsResponse.class));
            }
            if(productSequenceConfigList == null) {
                response.setProductSequenceConfig(convertToDtoList(oldProductSequenceConfigList, ProductSequenceConfigResponse.class));
            }

            if(hblTermsConditionTemplateList != null) {
                List<HblTermsConditionTemplate> hblTermsConditionTemplates = hblTermsConditionTemplateDao.updateEntityFromSettings(convertToEntityList(hblTermsConditionTemplateList, HblTermsConditionTemplate.class), shipmentSettingsDetails.getId(), true);
                response.setHblTermsConditionTemplate(convertToDtoList(hblTermsConditionTemplates, HblTermsConditionTemplateResponse.class));
            }
            if(hblHawbBackPrintTemplateList != null) {
                List<HblTermsConditionTemplate> hblHawbBackPrintTemplates = hblTermsConditionTemplateDao.updateEntityFromSettings(convertToEntityList(hblHawbBackPrintTemplateList, HblTermsConditionTemplate.class), shipmentSettingsDetails.getId(), false);
                response.setHblHawbBackPrintTemplate(convertToDtoList(hblHawbBackPrintTemplates, HblTermsConditionTemplateResponse.class));
            }
            if(tenantProductsList != null) {
                List<TenantProducts> tenantProducts = tenantProductsDao.updateEntityFromSettings(convertToEntityList(tenantProductsList, TenantProducts.class), shipmentSettingsDetails.getId());
                response.setTenantProducts(convertToDtoList(tenantProducts, TenantProductsResponse.class));
            }
            if(productSequenceConfigList != null) {
                if(productSequenceConfigList.size() > 0) {
                    for (ProductSequenceConfigRequest productSequenceConfig: productSequenceConfigList) {
                        if(productSequenceConfig.getTenantProducts() != null && productSequenceConfig.getTenantProducts().getProductType() != null) {
                            ListCommonRequest listCommonRequest = constructListCommonRequest("productType", stringValueOf(productSequenceConfig.getTenantProducts().getProductType()), "=");
                            Pair<Specification<TenantProducts>, Pageable> pair = fetchData(listCommonRequest, TenantProducts.class);
                            Page<TenantProducts> tenantProducts = tenantProductsDao.findAll(pair.getLeft(), pair.getRight());
                            productSequenceConfig.setTenantProducts(convertToClass(tenantProducts.getContent().get(0), TenantProductsRequest.class));
                        }
                        else
                            productSequenceConfig.setTenantProducts(null);
                    }
                }
                List<ProductSequenceConfig> productSequenceConfigs = productSequenceConfigDao.updateEntityFromSettings(convertToEntityList(productSequenceConfigList, ProductSequenceConfig.class), shipmentSettingsDetails.getId());
                response.setProductSequenceConfig(convertToDtoList(productSequenceConfigs, ProductSequenceConfigResponse.class));
            }

            try{
                shipmentSettingsSync.sync(shipmentSettingsDetails);
            } catch (Exception e) {
                log.error("Error Syncing Tenant Settings");
            }

            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            TransactionAspectSupport.currentTransactionStatus().setRollbackOnly();
            throw new RuntimeException(e);
        }
    }

    @Transactional
    @Override
    public ResponseEntity<?> completeSettingsUpdateCreateV1(CommonRequestModel commonRequestModel) throws Exception {
        String responseMsg;
        ShipmentSettingRequest request = (ShipmentSettingRequest) commonRequestModel.getData();
        if(request == null) {
            log.error("Request is empty for Shipment Settings update with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }

        if(request.getTenantId() == null) {
            log.error("Request Tenant Id is null for Shipment Settings update with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }

        Optional<ShipmentSettingsDetails> oldEntity = null;
        if(request.getTenantId() != null) {
            ListCommonRequest newRequest = new ListCommonRequest();
            newRequest.setPageNo(1);
            newRequest.setPageSize(Integer.MAX_VALUE);
            newRequest.setFilterCriteria(new ArrayList<>());
            Pair<Specification<ShipmentSettingsDetails>, Pageable> tuple = fetchData(newRequest, ShipmentSettingsDetails.class);
            Page<ShipmentSettingsDetails> shipmentSettingsPage = shipmentSettingsDao.list(tuple.getLeft(), tuple.getRight());
            if(shipmentSettingsPage.get().collect(Collectors.toList()) != null && shipmentSettingsPage.get().collect(Collectors.toList()).size() > 0)
                oldEntity = Optional.ofNullable(shipmentSettingsPage.get().collect(Collectors.toList()).get(0));
        }
        else {
            long id = request.getId();
            oldEntity = shipmentSettingsDao.findById(id);
        }
        if(oldEntity == null || !oldEntity.isPresent()) {
            try{
                return completeCreateFromV1(commonRequestModel);
            } catch (Exception e) {
                responseMsg = e.getMessage() != null ? e.getMessage()
                        : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
                log.error(responseMsg, e);
                TransactionAspectSupport.currentTransactionStatus().setRollbackOnly();
                throw new RuntimeException(e);
            }
        }
        else {
            try{
                return completeUpdateFromV1(oldEntity, commonRequestModel);
            } catch (Exception e) {
                responseMsg = e.getMessage() != null ? e.getMessage()
                        : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
                log.error(responseMsg, e);
                TransactionAspectSupport.currentTransactionStatus().setRollbackOnly();
                throw new RuntimeException(e);
            }
        }
    }

    public ResponseEntity<?> completeCreateFromV1(CommonRequestModel commonRequestModel) throws Exception {
        String responseMsg;
        ShipmentSettingRequest request = null;
        request = (ShipmentSettingRequest) commonRequestModel.getData();
        if(request == null) {
            log.debug("Request is empty for Shipment Settings create with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }
        ShipmentSettingsDetails shipmentSettingsDetails = convertRequestToEntity(request);
        try {
            shipmentSettingsDetails = shipmentSettingsDao.save(shipmentSettingsDetails);

            if(shipmentSettingsDetails.getHblTermsConditionTemplate() != null) {
                shipmentSettingsDetails.setHblTermsConditionTemplate(hblTermsConditionTemplateDao.saveEntityFromSettings(shipmentSettingsDetails.getHblTermsConditionTemplate(), shipmentSettingsDetails.getId(), true));
            }
            if(shipmentSettingsDetails.getHblHawbBackPrintTemplate() != null) {
                shipmentSettingsDetails.setHblHawbBackPrintTemplate(hblTermsConditionTemplateDao.saveEntityFromSettings(shipmentSettingsDetails.getHblHawbBackPrintTemplate(), shipmentSettingsDetails.getId(), false));
            }
            if(shipmentSettingsDetails.getTenantProducts() != null) {
                shipmentSettingsDetails.setTenantProducts(tenantProductsDao.saveEntityFromSettings(shipmentSettingsDetails.getTenantProducts(), shipmentSettingsDetails.getId()));
            }
            if(shipmentSettingsDetails.getProductSequenceConfig() != null) {
                if(shipmentSettingsDetails.getProductSequenceConfig().size() > 0) {
                    for (ProductSequenceConfig productSequenceConfig: shipmentSettingsDetails.getProductSequenceConfig()) {
                        ListCommonRequest listCommonRequest = constructListCommonRequest("productType", String.valueOf(productSequenceConfig.getTenantProducts().getProductType()), "=");
                        Pair<Specification<TenantProducts>, Pageable> pair = fetchData(listCommonRequest, TenantProducts.class);
                        Page<TenantProducts> tenantProducts = tenantProductsDao.findAll(pair.getLeft(), pair.getRight());
                        productSequenceConfig.setTenantProducts(tenantProducts.getContent().get(0));
                    }
                }
                shipmentSettingsDetails.setProductSequenceConfig(productSequenceConfigDao.saveEntityFromSettings(shipmentSettingsDetails.getProductSequenceConfig(), shipmentSettingsDetails.getId()));
            }

            try{
                shipmentSettingsSync.sync(shipmentSettingsDetails);
            } catch (Exception e) {
                log.error("Error Syncing Tenant Settings");
            }

            log.info("Shipment Setting Details created successfully for Id {} with Request Id {}", shipmentSettingsDetails.getId(), LoggerHelper.getRequestIdFromMDC());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            TransactionAspectSupport.currentTransactionStatus().setRollbackOnly();
            throw new RuntimeException(e);
        }
        return ResponseHelper.buildSuccessResponse(convertEntityToDto(shipmentSettingsDetails));
    }
    public ResponseEntity<?> completeUpdateFromV1(Optional<ShipmentSettingsDetails> oldEntity, CommonRequestModel commonRequestModel) throws Exception {
        String responseMsg;
        ShipmentSettingRequest request = (ShipmentSettingRequest) commonRequestModel.getData();

        try {
            request.setId(oldEntity.get().getId());
            request.setGuid(oldEntity.get().getGuid());
            if(request.getHawbLockSettings() != null && oldEntity.get().getHawbLockSettings() != null) {
                request.getHawbLockSettings().setId(oldEntity.get().getHawbLockSettings().getId());
                request.getHawbLockSettings().setGuid(oldEntity.get().getHawbLockSettings().getGuid());
            }
            if(request.getMawbLockSettings() != null && oldEntity.get().getMawbLockSettings() != null) {
                request.getMawbLockSettings().setId(oldEntity.get().getMawbLockSettings().getId());
                request.getMawbLockSettings().setGuid(oldEntity.get().getMawbLockSettings().getGuid());
            }
            if(request.getHblLockSettings() != null && oldEntity.get().getHblLockSettings() != null) {
                request.getHblLockSettings().setId(oldEntity.get().getHblLockSettings().getId());
                request.getHblLockSettings().setGuid(oldEntity.get().getHblLockSettings().getGuid());
            }
            ShipmentSettingsDetails shipmentSettingsDetails = convertRequestToEntity(request);

            List<HblTermsConditionTemplateRequest> hblTermsConditionTemplateList = request.getHblTermsConditionTemplate();
            List<HblTermsConditionTemplateRequest> hblHawbBackPrintTemplateList = request.getHblHawbBackPrintTemplate();
            List<TenantProductsRequest> tenantProductsList = request.getTenantProducts();
            List<ProductSequenceConfigRequest> productSequenceConfigList = request.getProductSequenceConfig();

            List<HblTermsConditionTemplate> oldHblTermsConditionTemplateList = oldEntity.get().getHblTermsConditionTemplate();
            List<HblTermsConditionTemplate> oldHblHawbBackPrintTemplateList = oldEntity.get().getHblHawbBackPrintTemplate();
            List<TenantProducts> oldTenantProductsList = oldEntity.get().getTenantProducts();
            List<ProductSequenceConfig> oldProductSequenceConfigList = oldEntity.get().getProductSequenceConfig();

            shipmentSettingsDetails = shipmentSettingsDao.save(shipmentSettingsDetails);

            ShipmentSettingsDetailsResponse response = jsonHelper.convertValue(shipmentSettingsDetails, ShipmentSettingsDetailsResponse.class);
            response.setTenantId(oldEntity.get().getTenantId());
            if(hblTermsConditionTemplateList == null) {
                response.setHblTermsConditionTemplate(convertToDtoList(oldHblTermsConditionTemplateList, HblTermsConditionTemplateResponse.class));
            }
            if(hblHawbBackPrintTemplateList == null) {
                response.setHblHawbBackPrintTemplate(convertToDtoList(oldHblHawbBackPrintTemplateList, HblTermsConditionTemplateResponse.class));
            }
            if(tenantProductsList == null) {
                response.setTenantProducts(convertToDtoList(oldTenantProductsList, TenantProductsResponse.class));
            }
            if(productSequenceConfigList == null) {
                response.setProductSequenceConfig(convertToDtoList(oldProductSequenceConfigList, ProductSequenceConfigResponse.class));
            }

            if(hblTermsConditionTemplateList != null) {
                List<HblTermsConditionTemplate> hblTermsConditionTemplates = hblTermsConditionTemplateDao.updateEntityFromSettings(convertToEntityList(hblTermsConditionTemplateList, HblTermsConditionTemplate.class), shipmentSettingsDetails.getId(), true);
                response.setHblTermsConditionTemplate(convertToDtoList(hblTermsConditionTemplates, HblTermsConditionTemplateResponse.class));
            }
            if(hblHawbBackPrintTemplateList != null) {
                List<HblTermsConditionTemplate> hblHawbBackPrintTemplates = hblTermsConditionTemplateDao.updateEntityFromSettings(convertToEntityList(hblHawbBackPrintTemplateList, HblTermsConditionTemplate.class), shipmentSettingsDetails.getId(), false);
                response.setHblHawbBackPrintTemplate(convertToDtoList(hblHawbBackPrintTemplates, HblTermsConditionTemplateResponse.class));
            }
            if(tenantProductsList != null) {
                List<TenantProducts> tenantProducts = tenantProductsDao.updateEntityFromSettings(convertToEntityList(tenantProductsList, TenantProducts.class), shipmentSettingsDetails.getId());
                response.setTenantProducts(convertToDtoList(tenantProducts, TenantProductsResponse.class));
            }
            if(productSequenceConfigList != null) {
                if(productSequenceConfigList.size() > 0) {
                    for (ProductSequenceConfigRequest productSequenceConfig: productSequenceConfigList) {
                        if(productSequenceConfig.getTenantProducts() != null && productSequenceConfig.getTenantProducts().getProductType() != null) {
                            ListCommonRequest listCommonRequest = constructListCommonRequest("productType", stringValueOf(productSequenceConfig.getTenantProducts().getProductType()), "=");
                            Pair<Specification<TenantProducts>, Pageable> pair = fetchData(listCommonRequest, TenantProducts.class);
                            Page<TenantProducts> tenantProducts = tenantProductsDao.findAll(pair.getLeft(), pair.getRight());
                            productSequenceConfig.setTenantProducts(convertToClass(tenantProducts.getContent().get(0), TenantProductsRequest.class));
                        }
                        else
                            productSequenceConfig.setTenantProducts(null);
                    }
                }
                List<ProductSequenceConfig> productSequenceConfigs = productSequenceConfigDao.updateEntityFromSettings(convertToEntityList(productSequenceConfigList, ProductSequenceConfig.class), shipmentSettingsDetails.getId());
                response.setProductSequenceConfig(convertToDtoList(productSequenceConfigs, ProductSequenceConfigResponse.class));
            }

            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            TransactionAspectSupport.currentTransactionStatus().setRollbackOnly();
            throw new RuntimeException(e);
        }
    }

    public ResponseEntity<?> retrieveById(CommonRequestModel commonRequestModel){
        String responseMsg;
        try {
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            if(request == null) {
                log.error("Request is empty for Shipment Settings retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            if(request.getId() == null && request.getGuid() == null) {
                log.error("Request Id and Guid is null for Shipment Settings retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            if(request.getId() == null) {
                log.error("Request Id is null for Shipment Settings retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            if(request.getGuid() == null) {
                log.error("GUID is null for Shipment Settings retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            ShipmentSettingsDetailsResponse response;
            Optional<ShipmentSettingsDetails> shipmentSettingsDetails;
            if(request.getId() != null) {
                long id = request.getId();
                shipmentSettingsDetails = shipmentSettingsDao.findById(id);
                if(!shipmentSettingsDetails.isPresent()) {
                    log.debug("Shipment Setting is null for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
                    throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
                }
                log.info("Shipment Settings details fetched successfully for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());

            } else {
                UUID guid = UUID.fromString(request.getGuid());
                shipmentSettingsDetails = shipmentSettingsDao.findByGuid(guid);
                if(!shipmentSettingsDetails.isPresent()) {
                    log.debug("Shipment Setting is null for Guid {} with Request Id {}", guid, LoggerHelper.getRequestIdFromMDC());
                    throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
                }
                log.info("Shipment Settings details fetched successfully for Guid {} with Request Id {}", guid, LoggerHelper.getRequestIdFromMDC());
            }
            response = convertEntityToDto(shipmentSettingsDetails.get());
            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }


    public ResponseEntity<?> list(CommonRequestModel commonRequestModel){
        String responseMsg;
        try {
            ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
            if(request == null) {
                log.error("Request is empty for Shipment Settings list with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            Pair<Specification<ShipmentSettingsDetails>, Pageable> tuple = fetchData(request, ShipmentSettingsDetails.class);
            Page<ShipmentSettingsDetails> shipmentSettingsPage = shipmentSettingsDao.list(tuple.getLeft(), tuple.getRight());
            log.info("Shipment Settings list retrieved successfully for Request Id {} ", LoggerHelper.getRequestIdFromMDC());
            return ResponseHelper.buildListSuccessResponse(
                    convertEntityListToDtoList(shipmentSettingsPage.getContent()),
                    shipmentSettingsPage.getTotalPages(),
                    shipmentSettingsPage.getTotalElements());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }

    }
    @Override
    @Async
    public CompletableFuture<ResponseEntity<?>> listAsync(CommonRequestModel commonRequestModel){
        String responseMsg;
        try {
            ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
            if(request == null) {
                log.error("Request is empty for Shipment Settings async list with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            Pair<Specification<ShipmentSettingsDetails>, Pageable> tuple = fetchData(request, ShipmentSettingsDetails.class);
            Page<ShipmentSettingsDetails> shipmentSettingsPage  = shipmentSettingsDao.list(tuple.getLeft(), tuple.getRight());
            log.info("Shipment Settings list retrieved successfully for Request Id {} ", LoggerHelper.getRequestIdFromMDC());
            return CompletableFuture.completedFuture(ResponseHelper.buildListSuccessResponse(
                    convertEntityListToDtoList(shipmentSettingsPage.getContent()),
                    shipmentSettingsPage.getTotalPages(),
                    shipmentSettingsPage.getTotalElements()));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return CompletableFuture.completedFuture(ResponseHelper.buildFailedResponse(responseMsg));
        }
    }

    @Override
    public ResponseEntity<?> delete(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            if(request == null) {
                log.debug("Request is empty for Shipment Settings delete with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            if(request.getId() == null && request.getGuid() == null) {
                log.error("Request Id and Guid is null for Shipment Settings delete with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            if(request.getId() == null) {
                log.error("Request Id is null for Shipment Settings delete with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            if(request.getGuid() == null) {
                log.error("GUID is null for Shipment Settings delete with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            if(request.getId() != null) {
                long id = request.getId();
                Optional<ShipmentSettingsDetails> note = shipmentSettingsDao.findById(id);
                if (note.isEmpty()) {
                    log.debug("ShipmentSettingsDetails is null for Id {} with Request Id {}", request.getId(), LoggerHelper.getRequestIdFromMDC());
                    throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
                }
                shipmentSettingsDao.delete(note.get());
                log.info("Deleted Shipment Settings for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
            } else {
                UUID guid = UUID.fromString(request.getGuid());
                Optional<ShipmentSettingsDetails> shipmentSettingsDetails = shipmentSettingsDao.findByGuid(guid);
                if (shipmentSettingsDetails.isEmpty()) {
                    log.debug("ShipmentSettingsDetails is null for Guid {} with Request Id {}", guid, LoggerHelper.getRequestIdFromMDC());
                    throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
                }
                shipmentSettingsDao.delete(shipmentSettingsDetails.get());
                log.info("Deleted Shipment Settings for Guid {} with Request Id {}", guid, LoggerHelper.getRequestIdFromMDC());
            }
            return ResponseHelper.buildSuccessResponse();
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_DELETE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }
    public ShipmentSettingsDetails convertRequestToEntity(ShipmentSettingRequest request) {
        return jsonHelper.convertValue(request, ShipmentSettingsDetails.class);
    }

    private ShipmentSettingsDetailsResponse convertEntityToDto(ShipmentSettingsDetails shipmentSettingsDetails) {
        return jsonHelper.convertValue(shipmentSettingsDetails, ShipmentSettingsDetailsResponse.class);
    }

    private List<IRunnerResponse> convertEntityListToDtoList(List<ShipmentSettingsDetails> list) {
        List<IRunnerResponse> responseList = new ArrayList<>();
        list.forEach(shipmentSettingsDetail -> {
            responseList.add(convertEntityToDto(shipmentSettingsDetail));
        });
        return responseList;
    }

    @Override
    public ResponseEntity<?> uploadTemplate(CommonRequestModel commonRequestModel) {
        TemplateUploadRequest templateUploadRequest = (TemplateUploadRequest) commonRequestModel.getData();
        if(templateUploadRequest.getPreviousFileId() == null || templateUploadRequest.getPreviousFileId().length() == 0) {
            try {
                ResponseEntity<TemplateUploadResponse> response = documentService.CreateDocumentTemplate(templateUploadRequest);
                if(response.getStatusCode() != HttpStatus.CREATED) {
                    LoggerHelper.error("Error While Uploading Template To Document Service");
                    String responseMsg = ShipmentSettingsConstants.UPLOAD_TEMPLATE_FAILED + " : " + response.getBody();
                    return ResponseHelper.buildFailedResponse(responseMsg);
                }
                return ResponseHelper.buildSuccessResponse(response.getBody());
            }
            catch (Exception e){
                LoggerHelper.error("Error While Uploading Template To Document Service");
                String responseMsg = e.getMessage() != null ? e.getMessage()
                        : ShipmentSettingsConstants.UPLOAD_TEMPLATE_FAILED;
                return ResponseHelper.buildFailedResponse(responseMsg);
            }
        }
        else{
            try {
                ResponseEntity<?> response = documentService.UpdateDocumentTemplate(templateUploadRequest);
                if(response.getStatusCode() != HttpStatus.OK){
                    LoggerHelper.error("Error While Updating Template To Document Service");
                    String responseMsg = ShipmentSettingsConstants.UPDATE_TEMPLATE_FAILED + " : " + response.getBody();
                    return ResponseHelper.buildFailedResponse(responseMsg);
                }
                TemplateUploadResponse templateUploadResponse = TemplateUploadResponse.builder()
                        .templateId(templateUploadRequest.getPreviousFileId()).build();
                return ResponseHelper.buildSuccessResponse(templateUploadResponse);
            } catch (Exception e) {
                LoggerHelper.error("Error While Uploading Template To Document Service");
                String responseMsg = e.getMessage() != null ? e.getMessage()
                        : ShipmentSettingsConstants.UPDATE_TEMPLATE_FAILED;
                log.error(responseMsg, e);
                return ResponseHelper.buildFailedResponse(responseMsg);
            }
        }
    }
    @Override
    public ResponseEntity<?> downloadTemplate(String templateId) {
        try {
            ResponseEntity<?> response = documentService.DownloadTemplate(templateId);
            if(response.getStatusCode() != HttpStatus.OK){
                LoggerHelper.error("Error While Downloading Template From Document Service");
                String responseMsg = ShipmentSettingsConstants.DOWNLOAD_TEMPLATE_FAILED + " : " + response.getBody();
                return ResponseHelper.buildFailedResponse(responseMsg);
            }
            return response;
        } catch (Exception e) {
            LoggerHelper.error("Error While Downloading Template From Document Service");
            String responseMsg = e.getMessage() != null ? e.getMessage()
                    : ShipmentSettingsConstants.DOWNLOAD_TEMPLATE_FAILED;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @Override
    public ResponseEntity<?> retrieveByTenantId(CommonRequestModel commonRequestModel){
        String responseMsg;
        try {
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            if (request == null || Objects.isNull(request.getId())) {
                log.error("Request is empty for Shipment Settings retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            Optional<ShipmentSettingsDetails> shipmentSettingsDetails = shipmentSettingsDao.findByTenantId(request.getId().intValue());
            if (!shipmentSettingsDetails.isPresent()) {
                log.debug("Shipment Setting is null for tenant id {} with Request Id {}", request.getId(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            log.info("Shipment Settings details fetched successfully for tenant id {} with Request Id {}", request.getId(), LoggerHelper.getRequestIdFromMDC());
            ShipmentSettingsDetailsResponse response = convertEntityToDto(shipmentSettingsDetails.get());
            return ResponseHelper.buildSuccessResponse(response);
        }  catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }
}
