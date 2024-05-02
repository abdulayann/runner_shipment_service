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
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.repository.interfaces.IShipmentSettingsRepository;
import com.dpw.runner.shipment.services.service.interfaces.IShipmentSettingsService;
import com.dpw.runner.shipment.services.service_bus.AzureServiceBusTopic;
import com.dpw.runner.shipment.services.service_bus.ISBProperties;
import com.dpw.runner.shipment.services.service_bus.ISBUtils;
import com.dpw.runner.shipment.services.syncing.interfaces.IShipmentSettingsSync;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
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
    private ISBUtils sbUtils;
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
    public ResponseEntity<IRunnerResponse> create(CommonRequestModel commonRequestModel) {
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
                        ListCommonRequest listCommonRequest = constructListCommonRequest(ShipmentSettingsConstants.PRODUCT_TYPE, productSequenceConfig.getTenantProducts().getProductType(), "=");
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
            throw new RuntimeException(e);
        }
        return ResponseHelper.buildSuccessResponse(convertEntityToDto(shipmentSettingsDetails));
    }

    @Transactional
    public ResponseEntity<IRunnerResponse> update(CommonRequestModel commonRequestModel) {
        return null;
    }

    @Transactional
    @Override
    public ResponseEntity<IRunnerResponse> completeUpdate(CommonRequestModel commonRequestModel) throws RunnerException {
        String responseMsg;
        ShipmentSettingRequest request = (ShipmentSettingRequest) commonRequestModel.getData();
        if(request == null) {
            log.error(ShipmentSettingsConstants.UPDATE_REQUEST_EMPTY, LoggerHelper.getRequestIdFromMDC());
        }

        if(request.getId() == null && request.getTenantId() == null) {
            log.error("Request Id and Tenant Id is null for Shipment Settings update with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }
        Optional<ShipmentSettingsDetails> oldEntity = Optional.empty();
        if(request.getTenantId() != null) {
            ListCommonRequest newRequest = new ListCommonRequest();
            newRequest.setPageNo(1);
            newRequest.setPageSize(Integer.MAX_VALUE);
            newRequest.setFilterCriteria(new ArrayList<>());
            Pair<Specification<ShipmentSettingsDetails>, Pageable> tuple = fetchData(newRequest, ShipmentSettingsDetails.class);
            Page<ShipmentSettingsDetails> shipmentSettingsPage = shipmentSettingsDao.list(tuple.getLeft(), tuple.getRight());
            if(shipmentSettingsPage.get().toList() != null && shipmentSettingsPage.get().toList().size() > 0)
                oldEntity = Optional.ofNullable(shipmentSettingsPage.get().toList().get(0));
        }
        else {
            long id = request.getId();
            oldEntity = shipmentSettingsDao.findById(id);
        }
        if(!oldEntity.isPresent()) {
            log.debug(ShipmentSettingsConstants.SHIPMENT_SETTINGS_RETRIEVE_BY_ID_ERROR, request.getId(), LoggerHelper.getRequestIdFromMDC());
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
                List<HblTermsConditionTemplate> hblTermsConditionTemplates = hblTermsConditionTemplateDao.updateEntityFromSettings(jsonHelper.convertValueToList(hblTermsConditionTemplateList, HblTermsConditionTemplate.class), shipmentSettingsDetails.getId(), true);
                response.setHblTermsConditionTemplate(jsonHelper.convertValueToList(hblTermsConditionTemplates, HblTermsConditionTemplateResponse.class));
            }
            if(hblHawbBackPrintTemplateList != null) {
                List<HblTermsConditionTemplate> hblHawbBackPrintTemplates = hblTermsConditionTemplateDao.updateEntityFromSettings(jsonHelper.convertValueToList(hblHawbBackPrintTemplateList, HblTermsConditionTemplate.class), shipmentSettingsDetails.getId(), false);
                response.setHblHawbBackPrintTemplate(jsonHelper.convertValueToList(hblHawbBackPrintTemplates, HblTermsConditionTemplateResponse.class));
            }
            if(tenantProductsList != null) {
                List<TenantProducts> tenantProducts = tenantProductsDao.updateEntityFromSettings(jsonHelper.convertValueToList(tenantProductsList, TenantProducts.class), shipmentSettingsDetails.getId());
                response.setTenantProducts(jsonHelper.convertValueToList(tenantProducts, TenantProductsResponse.class));
            }
            if(productSequenceConfigList != null) {
                if(productSequenceConfigList.size() > 0) {
                    for (ProductSequenceConfigRequest productSequenceConfig: productSequenceConfigList) {
                        if(productSequenceConfig.getTenantProducts() != null && productSequenceConfig.getTenantProducts().getProductType() != null) {
                            ListCommonRequest listCommonRequest = constructListCommonRequest(ShipmentSettingsConstants.PRODUCT_TYPE, stringValueOf(productSequenceConfig.getTenantProducts().getProductType()), "=");
                            Pair<Specification<TenantProducts>, Pageable> pair = fetchData(listCommonRequest, TenantProducts.class);
                            Page<TenantProducts> tenantProducts = tenantProductsDao.findAll(pair.getLeft(), pair.getRight());
                            productSequenceConfig.setTenantProducts(jsonHelper.convertValue(tenantProducts.getContent().get(0), TenantProductsRequest.class));
                        }
                        else
                            productSequenceConfig.setTenantProducts(null);
                    }
                }
                List<ProductSequenceConfig> productSequenceConfigs = productSequenceConfigDao.updateEntityFromSettings(jsonHelper.convertValueToList(productSequenceConfigList, ProductSequenceConfig.class), shipmentSettingsDetails.getId());
                response.setProductSequenceConfig(jsonHelper.convertValueToList(productSequenceConfigs, ProductSequenceConfigResponse.class));
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
    public ResponseEntity<IRunnerResponse> completeSettingsUpdateCreateV1(CommonRequestModel commonRequestModel) throws RunnerException {
        String responseMsg;
        ShipmentSettingRequest request = (ShipmentSettingRequest) commonRequestModel.getData();
        if(request == null) {
            log.error(ShipmentSettingsConstants.UPDATE_REQUEST_EMPTY, LoggerHelper.getRequestIdFromMDC());
        }

        if(request.getTenantId() == null) {
            log.error("Request Tenant Id is null for Shipment Settings update with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }

        Optional<ShipmentSettingsDetails> oldEntity = Optional.empty();
        if(request.getTenantId() != null) {
            ListCommonRequest newRequest = new ListCommonRequest();
            newRequest.setPageNo(1);
            newRequest.setPageSize(Integer.MAX_VALUE);
            newRequest.setFilterCriteria(new ArrayList<>());
            Pair<Specification<ShipmentSettingsDetails>, Pageable> tuple = fetchData(newRequest, ShipmentSettingsDetails.class);
            Page<ShipmentSettingsDetails> shipmentSettingsPage = shipmentSettingsDao.list(tuple.getLeft(), tuple.getRight());
            if(shipmentSettingsPage.get().toList() != null && shipmentSettingsPage.get().toList().size() > 0)
                oldEntity = Optional.ofNullable(shipmentSettingsPage.get().toList().get(0));
        }
        else {
            long id = request.getId();
            oldEntity = shipmentSettingsDao.findById(id);
        }
        if(!oldEntity.isPresent()) {
            try{
                return (ResponseEntity<IRunnerResponse>) completeCreateFromV1(commonRequestModel);
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

    public ResponseEntity<IRunnerResponse> completeCreateFromV1(CommonRequestModel commonRequestModel) throws RunnerException {
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
            List<ProductSequenceConfig> productSequenceConfigList = new ArrayList<>();
            if(shipmentSettingsDetails.getProductSequenceConfig() != null) {
                if(shipmentSettingsDetails.getProductSequenceConfig().size() > 0) {
                    for (ProductSequenceConfig productSequenceConfig: shipmentSettingsDetails.getProductSequenceConfig()) {
                        if(productSequenceConfig.getTenantProducts() != null && productSequenceConfig.getTenantProducts().getProductType() != null) {
                            ListCommonRequest listCommonRequest = constructListCommonRequest(ShipmentSettingsConstants.PRODUCT_TYPE, String.valueOf(productSequenceConfig.getTenantProducts().getProductType()), "=");
                            Pair<Specification<TenantProducts>, Pageable> pair = fetchData(listCommonRequest, TenantProducts.class);
                            Page<TenantProducts> tenantProducts = tenantProductsDao.findAll(pair.getLeft(), pair.getRight());
                            if(tenantProducts != null && !tenantProducts.isEmpty()) {
                                productSequenceConfig.setTenantProducts(tenantProducts.getContent().get(0));
                                productSequenceConfigList.add(productSequenceConfig);
                            }
                        }
                    }
                }
                shipmentSettingsDetails.setProductSequenceConfig(productSequenceConfigDao.saveEntityFromSettings(productSequenceConfigList, shipmentSettingsDetails.getId()));
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
    public ResponseEntity<IRunnerResponse> completeUpdateFromV1(Optional<ShipmentSettingsDetails> oldEntity, CommonRequestModel commonRequestModel) throws RunnerException {
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
                ListCommonRequest listCommonRequest = constructListCommonRequest("shipmentSettingsId", shipmentSettingsDetails.getId(), "=");
                Pair<Specification<TenantProducts>, Pageable> pair = fetchData(listCommonRequest, TenantProducts.class);
                Page<TenantProducts> tenantProductsPage = tenantProductsDao.findAll(pair.getLeft(), pair.getRight());
                if(tenantProductsPage != null && !tenantProductsPage.isEmpty())
                    oldTenantProductsList = tenantProductsPage.getContent();
                else
                    oldTenantProductsList = null;
                List<TenantProducts> tenantProducts = tenantProductsDao.updateEntityFromV1Settings(convertToEntityList(tenantProductsList, TenantProducts.class), shipmentSettingsDetails.getId(), oldTenantProductsList);
                response.setTenantProducts(convertToDtoList(tenantProducts, TenantProductsResponse.class));
            }
            if(productSequenceConfigList != null) {
                ListCommonRequest listCommonRequest = constructListCommonRequest("shipmentSettingsId", shipmentSettingsDetails.getId(), "=");
                Pair<Specification<ProductSequenceConfig>, Pageable> pair = fetchData(listCommonRequest, ProductSequenceConfig.class);
                Page<ProductSequenceConfig> productsPage = productSequenceConfigDao.findAll(pair.getLeft(), pair.getRight());
                if(productsPage != null && !productsPage.isEmpty())
                    oldProductSequenceConfigList = productsPage.getContent();
                else
                    oldProductSequenceConfigList = null;
                List<ProductSequenceConfigRequest> productSequenceConfigRequests = new ArrayList<>();
                if(productSequenceConfigList.size() > 0) {
                    for (ProductSequenceConfigRequest productSequenceConfig: productSequenceConfigList) {
                        if(productSequenceConfig.getTenantProducts() != null && productSequenceConfig.getTenantProducts().getProductType() != null) {
                            listCommonRequest = constructListCommonRequest(ShipmentSettingsConstants.PRODUCT_TYPE, stringValueOf(productSequenceConfig.getTenantProducts().getProductType()), "=");
                            Pair<Specification<TenantProducts>, Pageable> pair2 = fetchData(listCommonRequest, TenantProducts.class);
                            Page<TenantProducts> tenantProducts = tenantProductsDao.findAll(pair2.getLeft(), pair2.getRight());
                            if(tenantProducts != null && !tenantProducts.isEmpty()) {
                                productSequenceConfig.setTenantProducts(convertToClass(tenantProducts.getContent().get(0), TenantProductsRequest.class));
                                productSequenceConfigRequests.add(productSequenceConfig);
                            }
                        }
                        else
                            productSequenceConfig.setTenantProducts(null);
                    }
                }
                List<ProductSequenceConfig> productSequenceConfigs = productSequenceConfigDao.updateEntityFromV1Settings(convertToEntityList(productSequenceConfigRequests, ProductSequenceConfig.class), shipmentSettingsDetails.getId(), oldProductSequenceConfigList);
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

    public ResponseEntity<IRunnerResponse> retrieveById(CommonRequestModel commonRequestModel){
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
                    log.debug(ShipmentSettingsConstants.SHIPMENT_SETTINGS_RETRIEVE_BY_ID_ERROR, id, LoggerHelper.getRequestIdFromMDC());
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


    public ResponseEntity<IRunnerResponse> list(CommonRequestModel commonRequestModel){
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
    public CompletableFuture<ResponseEntity<IRunnerResponse>> listAsync(CommonRequestModel commonRequestModel){
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
    public ResponseEntity<IRunnerResponse> delete(CommonRequestModel commonRequestModel) {
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
    public ResponseEntity<IRunnerResponse> uploadTemplate(CommonRequestModel commonRequestModel) {
        TemplateUploadRequest templateUploadRequest = (TemplateUploadRequest) commonRequestModel.getData();
        if(templateUploadRequest.getPreviousFileId() == null || templateUploadRequest.getPreviousFileId().length() == 0) {
            try {
                ResponseEntity<TemplateUploadResponse> response = documentService.createDocumentTemplate(templateUploadRequest);
                if(response.getStatusCode() != HttpStatus.CREATED) {
                    LoggerHelper.error(ShipmentSettingsConstants.ERROR_UPLOADING_TEMPLATE);
                    String responseMsg = ShipmentSettingsConstants.UPLOAD_TEMPLATE_FAILED + " : " + response.getBody();
                    return ResponseHelper.buildFailedResponse(responseMsg);
                }
                return ResponseHelper.buildSuccessResponse(response.getBody());
            }
            catch (Exception e){
                LoggerHelper.error(ShipmentSettingsConstants.ERROR_UPLOADING_TEMPLATE);
                String responseMsg = e.getMessage() != null ? e.getMessage()
                        : ShipmentSettingsConstants.UPLOAD_TEMPLATE_FAILED;
                return ResponseHelper.buildFailedResponse(responseMsg);
            }
        }
        else{
            try {
                ResponseEntity<String> response = documentService.updateDocumentTemplate(templateUploadRequest);
                if(response.getStatusCode() != HttpStatus.OK){
                    LoggerHelper.error("Error While Updating Template To Document Service");
                    String responseMsg = ShipmentSettingsConstants.UPDATE_TEMPLATE_FAILED + " : " + response.getBody();
                    return ResponseHelper.buildFailedResponse(responseMsg);
                }
                TemplateUploadResponse templateUploadResponse = TemplateUploadResponse.builder()
                        .templateId(templateUploadRequest.getPreviousFileId()).build();
                return ResponseHelper.buildSuccessResponse(templateUploadResponse);
            } catch (Exception e) {
                LoggerHelper.error(ShipmentSettingsConstants.ERROR_UPLOADING_TEMPLATE);
                String responseMsg = e.getMessage() != null ? e.getMessage()
                        : ShipmentSettingsConstants.UPDATE_TEMPLATE_FAILED;
                log.error(responseMsg, e);
                return ResponseHelper.buildFailedResponse(responseMsg);
            }
        }
    }
    @Override
    public ResponseEntity<IRunnerResponse> downloadTemplate(String templateId) {
        try {
            byte[] response = documentService.downloadTemplate(templateId);
            return ResponseHelper.buildFileResponse(response, null, "DownloadDocument.docx");
        } catch (Exception e) {
            LoggerHelper.error("Error While Downloading Template From Document Service");
            String responseMsg = e.getMessage() != null ? e.getMessage()
                    : ShipmentSettingsConstants.DOWNLOAD_TEMPLATE_FAILED;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @Override
    public ResponseEntity<IRunnerResponse> retrieveByTenantId(CommonRequestModel commonRequestModel){
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
