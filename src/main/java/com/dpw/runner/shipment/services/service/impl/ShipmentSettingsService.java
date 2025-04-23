package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.DocumentService.DocumentService;
import com.dpw.runner.shipment.services.ReportingService.Models.TenantModel;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.constants.ShipmentSettingsConstants;
import com.dpw.runner.shipment.services.commons.requests.*;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.*;
import com.dpw.runner.shipment.services.dto.patchrequest.ShipmentSettingsPatchRequest;
import com.dpw.runner.shipment.services.dto.request.*;
import com.dpw.runner.shipment.services.dto.response.*;
import com.dpw.runner.shipment.services.entity.HblTermsConditionTemplate;
import com.dpw.runner.shipment.services.entity.ProductSequenceConfig;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.entity.TenantProducts;
import com.dpw.runner.shipment.services.exception.exceptions.GenericException;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.mapper.ShipmentSettingsMapper;
import com.dpw.runner.shipment.services.repository.interfaces.IShipmentSettingsRepository;
import com.dpw.runner.shipment.services.service.interfaces.ISectionVisibilityService;
import com.dpw.runner.shipment.services.service.interfaces.IShipmentSettingsService;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.service_bus.AzureServiceBusTopic;
import com.dpw.runner.shipment.services.service_bus.ISBProperties;
import com.dpw.runner.shipment.services.service_bus.ISBUtils;
import com.dpw.runner.shipment.services.syncing.interfaces.IShipmentSettingsSync;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import com.dpw.runner.shipment.services.utils.StringUtility;
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

import java.time.LocalDateTime;
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
    private CommonUtils commonUtils;
    @Autowired
    private ISBUtils sbUtils;
    @Autowired
    private ISBProperties isbProperties;
    @Autowired
    private AzureServiceBusTopic azureServiceBusTopic;
    @Autowired
    private MasterDataUtils masterDataUtils;
    @Autowired
    IShipmentSettingsSync shipmentSettingsSync;
    @Autowired
    IShipmentSettingsRepository shipmentSettingsRepository;
    @Autowired
    ModelMapper modelMapper;
    @Autowired
    private ShipmentSettingsMapper shipmentSettingsMapper;

    @Autowired
    private ISectionVisibilityService sectionVisibilityService;

    @Autowired
    private IV1Service iv1Service;

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
            shipmentSettingsDetails.setIsEntityTransferPrerequisiteEnabledDate(null);
            if(Boolean.TRUE.equals(shipmentSettingsDetails.getIsEntityTransferPrerequisiteEnabled())) {
                shipmentSettingsDetails.setIsEntityTransferPrerequisiteEnabledDate(LocalDateTime.now());
            }

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
                if(!shipmentSettingsDetails.getProductSequenceConfig().isEmpty()) {
                    for (ProductSequenceConfig productSequenceConfig: shipmentSettingsDetails.getProductSequenceConfig()) {
                        ListCommonRequest listCommonRequest = constructListCommonRequest(ShipmentSettingsConstants.PRODUCT_TYPE, productSequenceConfig.getTenantProducts().getProductType(), "=");
                        Pair<Specification<TenantProducts>, Pageable> pair = fetchData(listCommonRequest, TenantProducts.class);
                        Page<TenantProducts> tenantProducts = tenantProductsDao.findAll(pair.getLeft(), pair.getRight());
                        productSequenceConfig.setTenantProducts(tenantProducts.getContent().get(0));
                    }
                }
                shipmentSettingsDetails.setProductSequenceConfig(productSequenceConfigDao.saveEntityFromSettings(shipmentSettingsDetails.getProductSequenceConfig(), shipmentSettingsDetails.getId()));
            }

            shipmentSettingsSync(shipmentSettingsDetails);

            log.info("Shipment Setting Details created successfully for Id {} with Request Id {}", shipmentSettingsDetails.getId(), LoggerHelper.getRequestIdFromMDC());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            throw new GenericException(e);
        }
        return ResponseHelper.buildSuccessResponse(convertEntityToDto(shipmentSettingsDetails));
    }

    private void shipmentSettingsSync(ShipmentSettingsDetails shipmentSettingsDetails) {
        try{
            shipmentSettingsSync.sync(shipmentSettingsDetails);
        } catch (Exception e) {
            log.error("Error Syncing Tenant Settings");
        }
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
        Optional<ShipmentSettingsDetails> oldEntity = getOldEntityShipmentSettingsDetails(request);
        if(oldEntity.isEmpty()) {
            log.debug(ShipmentSettingsConstants.SHIPMENT_SETTINGS_RETRIEVE_BY_ID_ERROR, request.getId(), LoggerHelper.getRequestIdFromMDC());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }

        try {
            request.setId(oldEntity.get().getId());
            request.setGuid(oldEntity.get().getGuid());
            processLockSettings(request, oldEntity.get());
            if (Boolean.TRUE.equals(request.getIsEntityTransferPrerequisiteEnabled())) {
                request.setIsEntityTransferPrerequisiteEnabledDate(Boolean.TRUE.equals(oldEntity.get().getIsEntityTransferPrerequisiteEnabled()) ? oldEntity.get().getIsEntityTransferPrerequisiteEnabledDate() : LocalDateTime.now());
            } else {
                request.setIsEntityTransferPrerequisiteEnabledDate(null);
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
            processListUpdates(hblTermsConditionTemplateList, response, oldHblTermsConditionTemplateList, hblHawbBackPrintTemplateList, oldHblHawbBackPrintTemplateList, tenantProductsList, oldTenantProductsList, productSequenceConfigList, oldProductSequenceConfigList, shipmentSettingsDetails);
            processProductSequenceConfigList(productSequenceConfigList, shipmentSettingsDetails, response);

            shipmentSettingsSync(shipmentSettingsDetails);

            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            throw new GenericException(e);
        }
    }

    private Optional<ShipmentSettingsDetails> getOldEntityShipmentSettingsDetails(ShipmentSettingRequest request) {
        Optional<ShipmentSettingsDetails> oldEntity = Optional.empty();
        if(request.getTenantId() != null) {
            ListCommonRequest newRequest = new ListCommonRequest();
            newRequest.setPageNo(1);
            newRequest.setPageSize(Integer.MAX_VALUE);
            newRequest.setFilterCriteria(new ArrayList<>());
            Pair<Specification<ShipmentSettingsDetails>, Pageable> tuple = fetchData(newRequest, ShipmentSettingsDetails.class);
            Page<ShipmentSettingsDetails> shipmentSettingsPage = shipmentSettingsDao.list(tuple.getLeft(), tuple.getRight());
            if(shipmentSettingsPage.get().toList() != null && !shipmentSettingsPage.get().toList().isEmpty())
                oldEntity = Optional.ofNullable(shipmentSettingsPage.get().toList().get(0));
        }
        else {
            long id = request.getId();
            oldEntity = shipmentSettingsDao.findById(id);
        }
        return oldEntity;
    }

    private void processLockSettings(ShipmentSettingRequest request, ShipmentSettingsDetails oldEntity) {
        if(request.getHawbLockSettings() != null && oldEntity.getHawbLockSettings() != null) {
            request.getHawbLockSettings().setId(oldEntity.getHawbLockSettings().getId());
            request.getHawbLockSettings().setGuid(oldEntity.getHawbLockSettings().getGuid());
        }
        if(request.getMawbLockSettings() != null && oldEntity.getMawbLockSettings() != null) {
            request.getMawbLockSettings().setId(oldEntity.getMawbLockSettings().getId());
            request.getMawbLockSettings().setGuid(oldEntity.getMawbLockSettings().getGuid());
        }
        if(request.getHblLockSettings() != null && oldEntity.getHblLockSettings() != null) {
            request.getHblLockSettings().setId(oldEntity.getHblLockSettings().getId());
            request.getHblLockSettings().setGuid(oldEntity.getHblLockSettings().getGuid());
        }
    }

    private void processListUpdates(List<HblTermsConditionTemplateRequest> hblTermsConditionTemplateList, ShipmentSettingsDetailsResponse response, List<HblTermsConditionTemplate> oldHblTermsConditionTemplateList, List<HblTermsConditionTemplateRequest> hblHawbBackPrintTemplateList, List<HblTermsConditionTemplate> oldHblHawbBackPrintTemplateList, List<TenantProductsRequest> tenantProductsList, List<TenantProducts> oldTenantProductsList, List<ProductSequenceConfigRequest> productSequenceConfigList, List<ProductSequenceConfig> oldProductSequenceConfigList, ShipmentSettingsDetails shipmentSettingsDetails) throws RunnerException {
        if(hblTermsConditionTemplateList == null) {
            response.setHblTermsConditionTemplate(commonUtils.convertToDtoList(oldHblTermsConditionTemplateList, HblTermsConditionTemplateResponse.class));
        }
        if(hblHawbBackPrintTemplateList == null) {
            response.setHblHawbBackPrintTemplate(commonUtils.convertToDtoList(oldHblHawbBackPrintTemplateList, HblTermsConditionTemplateResponse.class));
        }
        if(tenantProductsList == null) {
            response.setTenantProducts(commonUtils.convertToDtoList(oldTenantProductsList, TenantProductsResponse.class));
        }
        if(productSequenceConfigList == null) {
            response.setProductSequenceConfig(commonUtils.convertToDtoList(oldProductSequenceConfigList, ProductSequenceConfigResponse.class));
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
    }

    private void processProductSequenceConfigList(List<ProductSequenceConfigRequest> productSequenceConfigList, ShipmentSettingsDetails shipmentSettingsDetails, ShipmentSettingsDetailsResponse response) throws RunnerException {
        if(productSequenceConfigList != null) {
            if(!productSequenceConfigList.isEmpty()) {
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
        log.info("CR-ID {} || Shipment Settings syncing from V1 with payload: {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(request));

        Optional<ShipmentSettingsDetails> oldEntity = getSettingsDetailsOptional(request);
        if(!oldEntity.isPresent()) {
            try{
                return completeCreateFromV1(commonRequestModel);
            } catch (Exception e) {
                responseMsg = e.getMessage() != null ? e.getMessage()
                        : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
                log.error(responseMsg, e);
                throw new GenericException(e);
            }
        }
        else {
            try{
                return completeUpdateFromV1(oldEntity, commonRequestModel);
            } catch (Exception e) {
                responseMsg = e.getMessage() != null ? e.getMessage()
                        : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
                log.error(responseMsg, e);
                throw new GenericException(e);
            }
        }
    }

    private Optional<ShipmentSettingsDetails> getSettingsDetailsOptional(ShipmentSettingRequest request) {
        Optional<ShipmentSettingsDetails> oldEntity = Optional.empty();
        if(request.getTenantId() != null) {
            List<ShipmentSettingsDetails> shipmentSettingsDetails = shipmentSettingsDao.getSettingsByTenantIds(List.of(request.getTenantId().intValue()));
            if(shipmentSettingsDetails != null && !shipmentSettingsDetails.isEmpty())
                oldEntity = Optional.ofNullable(shipmentSettingsDetails.get(0));
        }
        return oldEntity;
    }

    public ResponseEntity<IRunnerResponse> completeCreateFromV1(CommonRequestModel commonRequestModel) throws RunnerException {
        String responseMsg;
        ShipmentSettingRequest request = null;
        request = (ShipmentSettingRequest) commonRequestModel.getData();
        if(request == null) {
            log.debug("Request is empty for Shipment Settings create with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }
        request.setHideManifest(true);
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
            processProductSequenceConfigListForV1Create(shipmentSettingsDetails, productSequenceConfigList);

            log.info("Shipment Setting Details created successfully for Id {} with Request Id {}", shipmentSettingsDetails.getId(), LoggerHelper.getRequestIdFromMDC());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            throw new GenericException(e);
        }
        return ResponseHelper.buildSuccessResponse(convertEntityToDto(shipmentSettingsDetails));
    }

    private void processProductSequenceConfigListForV1Create(ShipmentSettingsDetails shipmentSettingsDetails, List<ProductSequenceConfig> productSequenceConfigList) {
        if(shipmentSettingsDetails.getProductSequenceConfig() == null) {
            return;
        }
        if(!shipmentSettingsDetails.getProductSequenceConfig().isEmpty()) {
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

    public ResponseEntity<IRunnerResponse> completeUpdateFromV1(Optional<ShipmentSettingsDetails> oldEntity, CommonRequestModel commonRequestModel) throws RunnerException {
        String responseMsg;
        ShipmentSettingRequest request = (ShipmentSettingRequest) commonRequestModel.getData();

        try {
            request.setId(oldEntity.get().getId());
            request.setGuid(oldEntity.get().getGuid());
            if(request.getPreAlertEmailAndLogs() == null) {
                request.setPreAlertEmailAndLogs(oldEntity.get().getPreAlertEmailAndLogs());
            }
            if(request.getHideManifest() == null) {
                request.setHideManifest(oldEntity.get().getHideManifest());
            }
            if(request.getIsEntityTransferPrerequisiteEnabled() == null) {
                request.setIsEntityTransferPrerequisiteEnabled(oldEntity.get().getIsEntityTransferPrerequisiteEnabled());
            }
            if(request.getIsEntityTransferPrerequisiteEnabledDate() == null && oldEntity.get().getIsEntityTransferPrerequisiteEnabledDate() !=null) {
                request.setIsEntityTransferPrerequisiteEnabledDate(oldEntity.get().getIsEntityTransferPrerequisiteEnabledDate());
            }
            if(request.getIsNetworkTransferEntityEnabled() == null)
                request.setIsNetworkTransferEntityEnabled(oldEntity.get().getIsNetworkTransferEntityEnabled());

            if(request.getIsAutomaticTransferEnabled() == null)
                request.setIsAutomaticTransferEnabled(oldEntity.get().getIsAutomaticTransferEnabled());

            setNteAdditionalEmailsFlag(oldEntity, request);

            if(request.getEventsRevampEnabled() == null) {
                request.setEventsRevampEnabled(oldEntity.get().getEventsRevampEnabled());
            }
            if(request.getIsAwbRevampEnabled() == null)
                request.setIsAwbRevampEnabled(oldEntity.get().getIsAwbRevampEnabled());
            processLockSettings(request, oldEntity.get());
            if(request.getIsAlwaysUtilization() == null) {
                request.setIsAlwaysUtilization(oldEntity.get().getIsAlwaysUtilization());
            }
            if(request.getIsUtilizationForContainerQuoted() == null) {
                request.setIsUtilizationForContainerQuoted(oldEntity.get().getIsUtilizationForContainerQuoted());
            }
            if(request.getHasNoUtilization() == null) {
                request.setHasNoUtilization(oldEntity.get().getHasNoUtilization());
            }
            ShipmentSettingsDetails shipmentSettingsDetails = convertRequestToEntity(request);

            return getfuncResponseEntity(oldEntity, request, shipmentSettingsDetails);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            throw new GenericException(e);
        }
    }

    private void setNteAdditionalEmailsFlag(Optional<ShipmentSettingsDetails> oldEntity, ShipmentSettingRequest request) {
        if(request.getIsNteAdditionalEmailsEnabled() == null)
            request.setIsNteAdditionalEmailsEnabled(oldEntity.get().getIsNteAdditionalEmailsEnabled());
    }

    private ResponseEntity<IRunnerResponse> getfuncResponseEntity(Optional<ShipmentSettingsDetails> oldEntity, ShipmentSettingRequest request, ShipmentSettingsDetails shipmentSettingsDetails) throws RunnerException {
        List<HblTermsConditionTemplateRequest> hblTermsConditionTemplateList = request.getHblTermsConditionTemplate();
        List<HblTermsConditionTemplateRequest> hblHawbBackPrintTemplateList = request.getHblHawbBackPrintTemplate();
        List<TenantProductsRequest> tenantProductsList = request.getTenantProducts();
        List<ProductSequenceConfigRequest> productSequenceConfigList = request.getProductSequenceConfig();

        List<HblTermsConditionTemplate> oldHblTermsConditionTemplateList = oldEntity.get().getHblTermsConditionTemplate(); //NOSONAR
        List<HblTermsConditionTemplate> oldHblHawbBackPrintTemplateList = oldEntity.get().getHblHawbBackPrintTemplate();
        List<TenantProducts> oldTenantProductsList = oldEntity.get().getTenantProducts();
        List<ProductSequenceConfig> oldProductSequenceConfigList = oldEntity.get().getProductSequenceConfig();

        shipmentSettingsDetails = shipmentSettingsDao.save(shipmentSettingsDetails);

        ShipmentSettingsDetailsResponse response = jsonHelper.convertValue(shipmentSettingsDetails, ShipmentSettingsDetailsResponse.class);
        response.setTenantId(oldEntity.get().getTenantId());
        if(hblTermsConditionTemplateList == null) {
            response.setHblTermsConditionTemplate(commonUtils.convertToDtoList(oldHblTermsConditionTemplateList, HblTermsConditionTemplateResponse.class));
        }
        if(hblHawbBackPrintTemplateList == null) {
            response.setHblHawbBackPrintTemplate(commonUtils.convertToDtoList(oldHblHawbBackPrintTemplateList, HblTermsConditionTemplateResponse.class));
        }
        if(tenantProductsList == null) {
            response.setTenantProducts(commonUtils.convertToDtoList(oldTenantProductsList, TenantProductsResponse.class));
        }
        if(productSequenceConfigList == null) {
            response.setProductSequenceConfig(commonUtils.convertToDtoList(oldProductSequenceConfigList, ProductSequenceConfigResponse.class));
        }

        if(hblTermsConditionTemplateList != null) {
            List<HblTermsConditionTemplate> hblTermsConditionTemplates = hblTermsConditionTemplateDao.updateEntityFromSettings(commonUtils.convertToEntityList(hblTermsConditionTemplateList, HblTermsConditionTemplate.class), shipmentSettingsDetails.getId(), true);
            response.setHblTermsConditionTemplate(commonUtils.convertToDtoList(hblTermsConditionTemplates, HblTermsConditionTemplateResponse.class));
        }
        if(hblHawbBackPrintTemplateList != null) {
            List<HblTermsConditionTemplate> hblHawbBackPrintTemplates = hblTermsConditionTemplateDao.updateEntityFromSettings(commonUtils.convertToEntityList(hblHawbBackPrintTemplateList, HblTermsConditionTemplate.class), shipmentSettingsDetails.getId(), false);
            response.setHblHawbBackPrintTemplate(commonUtils.convertToDtoList(hblHawbBackPrintTemplates, HblTermsConditionTemplateResponse.class));
        }
        processTenantProductsList(tenantProductsList, shipmentSettingsDetails, response);
        processV1SeqConfigList(productSequenceConfigList, shipmentSettingsDetails, response);

        return ResponseHelper.buildSuccessResponse(response);
    }

    private void processV1SeqConfigList(List<ProductSequenceConfigRequest> productSequenceConfigList, ShipmentSettingsDetails shipmentSettingsDetails, ShipmentSettingsDetailsResponse response) throws RunnerException {
        List<ProductSequenceConfig> oldProductSequenceConfigList;
        if(productSequenceConfigList != null) {
            ListCommonRequest listCommonRequest = constructListCommonRequest("shipmentSettingsId", shipmentSettingsDetails.getId(), "=");
            Pair<Specification<ProductSequenceConfig>, Pageable> pair = fetchData(listCommonRequest, ProductSequenceConfig.class);
            Page<ProductSequenceConfig> productsPage = productSequenceConfigDao.findAll(pair.getLeft(), pair.getRight());
            if(productsPage != null && !productsPage.isEmpty())
                oldProductSequenceConfigList = productsPage.getContent();
            else
                oldProductSequenceConfigList = null;
            processProductSequenceConfigListForV1Update(productSequenceConfigList, shipmentSettingsDetails, oldProductSequenceConfigList, response);
        }
    }

    private void processTenantProductsList(List<TenantProductsRequest> tenantProductsList, ShipmentSettingsDetails shipmentSettingsDetails, ShipmentSettingsDetailsResponse response) throws RunnerException {
        List<TenantProducts> oldTenantProductsList;
        if(tenantProductsList != null) {
            ListCommonRequest listCommonRequest = constructListCommonRequest("shipmentSettingsId", shipmentSettingsDetails.getId(), "=");
            Pair<Specification<TenantProducts>, Pageable> pair = fetchData(listCommonRequest, TenantProducts.class);
            Page<TenantProducts> tenantProductsPage = tenantProductsDao.findAll(pair.getLeft(), pair.getRight());
            if(tenantProductsPage != null && !tenantProductsPage.isEmpty())
                oldTenantProductsList = tenantProductsPage.getContent();
            else
                oldTenantProductsList = null;
            List<TenantProducts> tenantProducts = tenantProductsDao.updateEntityFromV1Settings(jsonHelper.convertValueToList(tenantProductsList, TenantProducts.class), shipmentSettingsDetails.getId(), oldTenantProductsList);
            response.setTenantProducts(commonUtils.convertToDtoList(tenantProducts, TenantProductsResponse.class));
        }
    }

    private void processProductSequenceConfigListForV1Update(List<ProductSequenceConfigRequest> productSequenceConfigList, ShipmentSettingsDetails shipmentSettingsDetails, List<ProductSequenceConfig> oldProductSequenceConfigList, ShipmentSettingsDetailsResponse response) throws RunnerException {
        ListCommonRequest listCommonRequest;
        List<ProductSequenceConfigRequest> productSequenceConfigRequests = new ArrayList<>();
        if(!productSequenceConfigList.isEmpty()) {
            for (ProductSequenceConfigRequest productSequenceConfig: productSequenceConfigList) {
                if(productSequenceConfig.getTenantProducts() != null && productSequenceConfig.getTenantProducts().getProductType() != null) {
                    listCommonRequest = constructListCommonRequest(ShipmentSettingsConstants.PRODUCT_TYPE, stringValueOf(productSequenceConfig.getTenantProducts().getProductType()), "=");
                    Pair<Specification<TenantProducts>, Pageable> pair2 = fetchData(listCommonRequest, TenantProducts.class);
                    Page<TenantProducts> tenantProducts = tenantProductsDao.findAll(pair2.getLeft(), pair2.getRight());
                    if(tenantProducts != null && !tenantProducts.isEmpty()) {
                        productSequenceConfig.setTenantProducts(commonUtils.convertToClass(tenantProducts.getContent().get(0), TenantProductsRequest.class));
                        productSequenceConfigRequests.add(productSequenceConfig);
                    }
                }
                else
                    productSequenceConfig.setTenantProducts(null);
            }
        }
        List<ProductSequenceConfig> productSequenceConfigs = productSequenceConfigDao.updateEntityFromV1Settings(commonUtils.convertToEntityList(productSequenceConfigRequests, ProductSequenceConfig.class), shipmentSettingsDetails.getId(), oldProductSequenceConfigList);
        response.setProductSequenceConfig(commonUtils.convertToDtoList(productSequenceConfigs, ProductSequenceConfigResponse.class));
    }

    public ResponseEntity<IRunnerResponse> retrieveById(CommonRequestModel commonRequestModel){
        String responseMsg;
        try {
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            if(request == null) {
                log.error("Request is empty for Shipment Settings retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            if(request.getId() == null || request.getGuid() == null) {
                log.error("Request Id or Guid is null for Shipment Settings retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
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
        return null;
    }
    @Override
    @Async
    public CompletableFuture<ResponseEntity<IRunnerResponse>> listAsync(CommonRequestModel commonRequestModel){
        return null;
    }

    @Override
    public ResponseEntity<IRunnerResponse> delete(CommonRequestModel commonRequestModel) {
        return null;
    }
    public ShipmentSettingsDetails convertRequestToEntity(ShipmentSettingRequest request) {
        return jsonHelper.convertValue(request, ShipmentSettingsDetails.class);
    }

    private ShipmentSettingsDetailsResponse convertEntityToDto(ShipmentSettingsDetails shipmentSettingsDetails) {
        return jsonHelper.convertValue(shipmentSettingsDetails, ShipmentSettingsDetailsResponse.class);
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
                String responseMsg = getResponseMsg(e);
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

    private String getResponseMsg(Exception e) {
        return e.getMessage() != null ? e.getMessage()
                : ShipmentSettingsConstants.UPLOAD_TEMPLATE_FAILED;
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
            if (request.getSectionRule() != null && request.getSectionRule()) {
                TenantModel tenantModel = modelMapper.map(iv1Service.retrieveTenant().getEntity(), TenantModel.class);
                response.setSectionVisibilityResponses(sectionVisibilityService.getByBranchCode(tenantModel.getCode()));
            }
            return ResponseHelper.buildSuccessResponse(response);
        }  catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @Override
    public ResponseEntity<IRunnerResponse> listCoLoadStationTenantIds() {
        var tenantSettings = commonUtils.getCurrentTenantSettings();
        Set<String> tenantIds = new HashSet<>();
        tenantIds.add(StringUtility.convertToString(UserContext.getUser().TenantId));
        if (Boolean.TRUE.equals(tenantSettings.getIsMAWBColoadingEnabled())
                && Boolean.TRUE.equals(tenantSettings.getIsColoadingMAWBStationEnabled())
                && !Objects.isNull(tenantSettings.getColoadingBranchIds())) {
            tenantIds.addAll(tenantSettings.getColoadingBranchIds().stream().map(Object::toString).toList());
        }
        Map<String, TenantModel> v1Data = masterDataUtils.fetchInTenantsList(tenantIds);
        List<TenantModel> listOfColoadStations = v1Data.values().stream().sorted(Comparator.comparing(TenantModel::getTenantName)).toList();
        return ResponseHelper.buildSuccessResponse(listOfColoadStations);
    }

    @Override
    public ResponseEntity<IRunnerResponse> listHubTenantIds() {
        var tenantSettings = commonUtils.getCurrentTenantSettings();
        Set<String> tenantIds = new HashSet<>();
        tenantIds.add(StringUtility.convertToString(UserContext.getUser().TenantId));
        if (Boolean.TRUE.equals(tenantSettings.getIsMAWBColoadingEnabled())) {
            tenantIds.addAll(commonUtils.fetchColoadingDetails().stream().map(x -> x.getParentTenantId().toString()).toList());
        }
        Map<String, TenantModel> v1Data = masterDataUtils.fetchInTenantsList(tenantIds);
        List<TenantModel> listOfHubStations = v1Data.values().stream().sorted(Comparator.comparing(TenantModel::getTenantName)).toList();
        return ResponseHelper.buildSuccessResponse(listOfHubStations);
    }

    @Transactional
    @Override
    public ResponseEntity<IRunnerResponse> hideManifest(boolean hideManifest) {
        Optional<ShipmentSettingsDetails> entity = Optional.empty();
        ListCommonRequest newRequest = new ListCommonRequest();
        newRequest.setPageNo(1);
        newRequest.setPageSize(Integer.MAX_VALUE);
        newRequest.setFilterCriteria(new ArrayList<>());
        Pair<Specification<ShipmentSettingsDetails>, Pageable> tuple = fetchData(newRequest, ShipmentSettingsDetails.class);
        Page<ShipmentSettingsDetails> shipmentSettingsPage = shipmentSettingsDao.list(tuple.getLeft(), tuple.getRight());
        if(!shipmentSettingsPage.get().toList().isEmpty())
            entity = Optional.ofNullable(shipmentSettingsPage.get().toList().get(0));

        if(entity.isEmpty()) {
            log.debug(ShipmentSettingsConstants.SHIPMENT_SETTINGS_RETRIEVE_BY_ID_ERROR, TenantContext.getCurrentTenant(), LoggerHelper.getRequestIdFromMDC());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }
        ShipmentSettingsDetails shipmentSettingsDetails = entity.get();
        shipmentSettingsDetails.setHideManifest(hideManifest);
        shipmentSettingsDao.save(shipmentSettingsDetails);
        return ResponseHelper.buildSuccessResponse();
    }

    @Override
    public ResponseEntity<IRunnerResponse> partialUpdate(CommonRequestModel commonRequestModel) {
        String responseMsg;
        ShipmentSettingsPatchRequest shipmentSettingsPatchRequest = (ShipmentSettingsPatchRequest) commonRequestModel.getData();
        if(shipmentSettingsPatchRequest == null) {
            log.error(ShipmentSettingsConstants.UPDATE_REQUEST_EMPTY, LoggerHelper.getRequestIdFromMDC());
        }
        if(shipmentSettingsPatchRequest.getId() == null && shipmentSettingsPatchRequest.getTenantId() == null) {
            log.error("Request Id and Tenant Id is null for Shipment Settings update with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }
        Optional<ShipmentSettingsDetails> oldEntity;
        if(shipmentSettingsPatchRequest.getTenantId() != null) {
            oldEntity = shipmentSettingsDao.findByTenantId(shipmentSettingsPatchRequest.getTenantId().intValue());
        }
        else {
            long id = shipmentSettingsPatchRequest.getId();
            oldEntity = shipmentSettingsDao.findById(id);
        }
        if(oldEntity.isEmpty()) {
            log.debug(ShipmentSettingsConstants.SHIPMENT_SETTINGS_RETRIEVE_BY_ID_ERROR, shipmentSettingsPatchRequest.getId(), LoggerHelper.getRequestIdFromMDC());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }
        try {
            ShipmentSettingsDetails oldShipmentSettingsDetails = oldEntity.get();
            Boolean oldEntityTransferFlag = oldShipmentSettingsDetails.getIsEntityTransferPrerequisiteEnabled();
            Boolean newEntityTransferFlag = (shipmentSettingsPatchRequest.getIsEntityTransferPrerequisiteEnabled() != null) && shipmentSettingsPatchRequest.getIsEntityTransferPrerequisiteEnabled().orElse(false);
            Boolean newNetworkTransferFlag = (shipmentSettingsPatchRequest.getIsNetworkTransferEntityEnabled() != null) && shipmentSettingsPatchRequest.getIsNetworkTransferEntityEnabled().orElse(false);
            Boolean newAutomaticTransferFlag = (shipmentSettingsPatchRequest.getIsAutomaticTransferEnabled() != null) && shipmentSettingsPatchRequest.getIsAutomaticTransferEnabled().orElse(false);
            Boolean newNteAdditionalEmailsFlag = (shipmentSettingsPatchRequest.getIsNteAdditionalEmailsEnabled() != null) && shipmentSettingsPatchRequest.getIsNteAdditionalEmailsEnabled().orElse(false);
            Boolean newAwbRevampFlag = (shipmentSettingsPatchRequest.getIsAwbRevampEnabled() !=null) && shipmentSettingsPatchRequest.getIsAwbRevampEnabled().orElse(false);
            LocalDateTime oldEntityTransferEnabledDate = oldShipmentSettingsDetails.getIsEntityTransferPrerequisiteEnabledDate();
            shipmentSettingsMapper.update(shipmentSettingsPatchRequest, oldShipmentSettingsDetails);
            oldShipmentSettingsDetails.setIsEntityTransferPrerequisiteEnabled(newEntityTransferFlag);
            oldShipmentSettingsDetails.setIsNetworkTransferEntityEnabled(newNetworkTransferFlag);
            oldShipmentSettingsDetails.setIsAutomaticTransferEnabled(newAutomaticTransferFlag);
            oldShipmentSettingsDetails.setIsNteAdditionalEmailsEnabled(newNteAdditionalEmailsFlag);
            oldShipmentSettingsDetails.setIsAwbRevampEnabled(newAwbRevampFlag);
            setIsEntityTransferPrerequisiteEnabledDate(newEntityTransferFlag, oldShipmentSettingsDetails, oldEntityTransferFlag, oldEntityTransferEnabledDate);
            ShipmentSettingsDetails newShipmentSettingsDetails = shipmentSettingsDao.save(oldShipmentSettingsDetails);
            ShipmentSettingsDetailsResponse response = jsonHelper.convertValue(newShipmentSettingsDetails, ShipmentSettingsDetailsResponse.class);
            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage() : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            throw new GenericException(e);
        }
    }

    private void setIsEntityTransferPrerequisiteEnabledDate(Boolean newEntityTransferFlag, ShipmentSettingsDetails oldShipmentSettingsDetails, Boolean oldEntityTransferFlag, LocalDateTime oldEntityTransferEnabledDate) {
        if(Boolean.TRUE.equals(newEntityTransferFlag)) {
            oldShipmentSettingsDetails.setIsEntityTransferPrerequisiteEnabledDate(Boolean.FALSE.equals(oldEntityTransferFlag) ? LocalDateTime.now(): oldEntityTransferEnabledDate);
        } else {
            oldShipmentSettingsDetails.setIsEntityTransferPrerequisiteEnabledDate(null);
        }
    }
}
