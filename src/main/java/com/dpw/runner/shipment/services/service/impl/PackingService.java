package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.constants.PackingConstants;
import com.dpw.runner.shipment.services.commons.enums.DBOperationType;
import com.dpw.runner.shipment.services.commons.requests.*;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.config.SyncConfig;
import com.dpw.runner.shipment.services.dao.interfaces.IConsolidationDetailsDao;
import com.dpw.runner.shipment.services.dao.interfaces.IContainerDao;
import com.dpw.runner.shipment.services.dao.interfaces.IPackingDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.dto.request.PackingRequest;
import com.dpw.runner.shipment.services.dto.response.ContainerResponse;
import com.dpw.runner.shipment.services.dto.response.PackingResponse;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.entity.Packing;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IPackingService;
import com.dpw.runner.shipment.services.syncing.Entity.BulkPackingRequestV2;
import com.dpw.runner.shipment.services.syncing.Entity.PackingRequestV2;
import com.dpw.runner.shipment.services.syncing.constants.SyncingConstants;
import com.dpw.runner.shipment.services.syncing.impl.PackingSync;
import com.dpw.runner.shipment.services.utils.CSVParsingUtil;
import com.dpw.runner.shipment.services.utils.StringUtility;
import com.dpw.runner.shipment.services.utils.UnitConversionUtility;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.http.ResponseEntity;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.transaction.interceptor.TransactionAspectSupport;

import javax.servlet.http.HttpServletResponse;
import java.io.PrintWriter;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;
import static com.dpw.runner.shipment.services.utils.CommonUtils.constructListCommonRequest;
import static com.dpw.runner.shipment.services.utils.UnitConversionUtility.convertUnit;

@Slf4j
@Service
public class PackingService implements IPackingService {
    @Autowired
    IPackingDao packingDao;

    @Autowired
    IContainerDao containersDao;
    @Autowired
    private JsonHelper jsonHelper;

    @Autowired
    private AuditLogService auditLogService;

    @Autowired
    private ObjectMapper objectMapper;

    @Autowired
    private ModelMapper modelMapper;

    @Autowired
    private IShipmentDao shipmentDao;

    @Autowired
    private IConsolidationDetailsDao consolidationDao;

    @Autowired
    private PackingSync packingSync;
    @Lazy
    @Autowired
    private SyncQueueService syncQueueService;
    @Autowired
    private SyncConfig syncConfig;

    private final CSVParsingUtil<Packing> parser = new CSVParsingUtil<>(Packing.class);

    @Transactional
    public ResponseEntity<?> create(CommonRequestModel commonRequestModel) {
        String responseMsg;
        PackingRequest request = null;
        request = (PackingRequest) commonRequestModel.getData();
        if (request == null) {
            log.debug("Request is empty for Packing create with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }
        Packing packing = convertRequestToEntity(request);
        try {
            packing = packingDao.save(packing);

            // audit logs
            auditLogService.addAuditLog(
                    AuditLogMetaData.builder()
                            .newData(packing)
                            .prevData(null)
                            .parent(Packing.class.getSimpleName())
                            .parentId(packing.getId())
                            .operation(DBOperationType.CREATE.name()).build()
            );

            log.info("Packing Details created successfully for Id {} with Request Id {}", packing.getId(), LoggerHelper.getRequestIdFromMDC());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
        return ResponseHelper.buildSuccessResponse(convertEntityToDto(packing));
    }

    @Override
    public void uploadPacking(BulkUploadRequest request) throws Exception {
        List<Packing> packingList = parser.parseCSVFile(request.getFile());
        packingList.stream().forEach(packing -> {
            packing.setConsolidationId(request.getConsolidationId());
            packing.setShipmentId(request.getShipmentId());
        });
        packingDao.saveAll(packingList);
        packingSync.sync(packingList, request.getConsolidationId(), request.getShipmentId());
    }

    @Override
    public void downloadPacking(HttpServletResponse response, BulkDownloadRequest request) throws Exception {
        List<Packing> result = new ArrayList<>();
        if (request.getShipmentId() != null) {
            ListCommonRequest req = constructListCommonRequest("shipmentId", Long.valueOf(request.getShipmentId()), "=");
            Pair<Specification<Packing>, Pageable> pair = fetchData(req, Packing.class);
            Page<Packing> packings = packingDao.findAll(pair.getLeft(), pair.getRight());
            List<Packing> packingList = packings.getContent();
            result.addAll(packingList);
        }

        if (request.getConsolidationId() != null) {
            ListCommonRequest req2 = constructListCommonRequest("consolidationId", Long.valueOf(request.getConsolidationId()), "=");
            Pair<Specification<Packing>, Pageable> pair = fetchData(req2, Packing.class);
            Page<Packing> packings = packingDao.findAll(pair.getLeft(), pair.getRight());
            List<Packing> packingList = packings.getContent();
            if (result.isEmpty()) {
                result.addAll(packingList);
            } else {
                result = result.stream().filter(result::contains).collect(Collectors.toList());
            }
        }
        LocalDateTime currentTime = LocalDateTime.now();
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyyMMddHHmmss");
        String timestamp = currentTime.format(formatter);
        String filenameWithTimestamp = "Packings_" + timestamp + ".xlsx";

        response.setContentType("text/csv");
        response.setHeader("Content-Disposition", "attachment; filename=" + filenameWithTimestamp);

        try (PrintWriter writer = response.getWriter()) {
            writer.println(parser.generateCSVHeaderForContainer());
            for (Packing packing : result) {
                writer.println(parser.formatPackingAsCSVLine(packing));
            }
        }
    }

    @Transactional
    public ResponseEntity<?> update(CommonRequestModel commonRequestModel) {
        String responseMsg;
        PackingRequest request = (PackingRequest) commonRequestModel.getData();
        if (request == null) {
            log.debug("Request is empty for Packing update with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }

        if (request.getId() == null) {
            log.debug("Request Id is null for Packing update with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }
        long id = request.getId();
        Optional<Packing> oldEntity = packingDao.findById(id);
        if (!oldEntity.isPresent()) {
            log.debug("Packing is null for Id {} with Request Id {}", request.getId(), LoggerHelper.getRequestIdFromMDC());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }

        Packing packing = convertRequestToEntity(request);
        packing.setId(oldEntity.get().getId());

        if(packing.getGuid() != null && !oldEntity.get().getGuid().equals(packing.getGuid())) {
            throw new RunnerException("Provided GUID doesn't match with the existing one !");
        }
        try {
            String oldEntityJsonString = jsonHelper.convertToJson(oldEntity.get());
            packing = packingDao.save(packing);

            // audit logs
            auditLogService.addAuditLog(
                    AuditLogMetaData.builder()
                            .newData(packing)
                            .prevData(jsonHelper.readFromJson(oldEntityJsonString, Packing.class))
                            .parent(Packing.class.getSimpleName())
                            .parentId(packing.getId())
                            .operation(DBOperationType.UPDATE.name()).build()
            );
            log.info("Updated the packing details for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
        return ResponseHelper.buildSuccessResponse(convertEntityToDto(packing));
    }

    public ResponseEntity<?> list(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
            if (request == null) {
                log.error("Request is empty for Packing list with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            // construct specifications for filter request
            Pair<Specification<Packing>, Pageable> tuple = fetchData(request, Packing.class);
            Page<Packing> packingPage = packingDao.findAll(tuple.getLeft(), tuple.getRight());
            log.info("Packing list retrieved successfully for Request Id {} ", LoggerHelper.getRequestIdFromMDC());
            return ResponseHelper.buildListSuccessResponse(
                    convertEntityListToDtoList(packingPage.getContent()),
                    packingPage.getTotalPages(),
                    packingPage.getTotalElements());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }

    }

    @Override
    @Async
    public CompletableFuture<ResponseEntity<?>> listAsync(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
            if (request == null) {
                log.error("Request is empty for Packing async list with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            // construct specifications for filter request
            Pair<Specification<Packing>, Pageable> tuple = fetchData(request, Packing.class);
            Page<Packing> packingPage = packingDao.findAll(tuple.getLeft(), tuple.getRight());
            log.info("Packing async list retrieved successfully for Request Id {} ", LoggerHelper.getRequestIdFromMDC());
            return CompletableFuture.completedFuture(
                    ResponseHelper
                            .buildListSuccessResponse(
                                    convertEntityListToDtoList(packingPage.getContent()),
                                    packingPage.getTotalPages(),
                                    packingPage.getTotalElements()));
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
        if (commonRequestModel == null) {
            log.debug("Request is empty for Packing delete with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }
        if (commonRequestModel.getId() == null) {
            log.debug("Request Id is null for Packing delete with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }
        Long id = commonRequestModel.getId();

        Optional<Packing> targetPacking = packingDao.findById(id);
        if (targetPacking.isEmpty()) {
            log.debug("No entity present for id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
            return ResponseHelper.buildFailedResponse(PackingConstants.NO_DATA);
        }
        try {
            String oldEntityJsonString = jsonHelper.convertToJson(targetPacking.get());
            packingDao.delete(targetPacking.get());

            // audit logs
            auditLogService.addAuditLog(
                    AuditLogMetaData.builder()
                            .newData(null)
                            .prevData(jsonHelper.readFromJson(oldEntityJsonString, Packing.class))
                            .parent(Packing.class.getSimpleName())
                            .parentId(targetPacking.get().getId())
                            .operation(DBOperationType.DELETE.name()).build()
            );
            log.info("Deleted packing for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_DELETE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
        return ResponseHelper.buildSuccessResponse();
    }

    @Override
    public ResponseEntity<?> retrieveById(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            if (request == null) {
                log.error("Request is empty for Packing retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            if (request.getId() == null) {
                log.error("Request Id is null for Packing retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            long id = request.getId();
            Optional<Packing> packing = packingDao.findById(id);
            if (packing.isEmpty()) {
                log.debug("Packing is null for Id {} with Request Id {}", request.getId(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            log.info("Packing details fetched successfully for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
            PackingResponse response = (PackingResponse) convertEntityToDto(packing.get());
            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @Override
    public ResponseEntity<?> calculateWeightVolumne(CommonRequestModel commonRequestModel) throws Exception {
        PackingRequest request = (PackingRequest) commonRequestModel.getData();
        Containers newContainer = null;

        List<IRunnerResponse> finalContainers = new ArrayList<>();

        if (request.getContainerId() != null) {

            newContainer = containersDao.findById(request.getContainerId()).get();

            if (request.getId() == null) {
                addWeightVolume(request, newContainer);
                finalContainers.add(convertEntityToDto(newContainer));
            } else {
                Packing packing = packingDao.findById(request.getId()).get();
                Containers oldContainer = containersDao.findById(packing.getContainerId()).get();

                Containers container = addWeightVolume(request, newContainer);

                if (oldContainer.getId() == newContainer.getId()) {
                    subtractWeightVolume(container.getAchievedWeight(), newContainer, packing, container.getAchievedVolume(), request);
                }
                if (oldContainer.getId() != newContainer.getId()) {
                    subtractWeightVolume(container.getAchievedWeight(), oldContainer, packing, container.getAchievedVolume(), request);
                    finalContainers.add(convertEntityToDto(oldContainer));
                }
                finalContainers.add(convertEntityToDto(newContainer));
            }
        }
        return ResponseHelper.buildListSuccessResponse(finalContainers);
    }

    @Override
    public ResponseEntity<?> listPacksToDetach(CommonRequestModel commonRequestModel) throws Exception {
        Long containerId = commonRequestModel.getId();
        ListCommonRequest listCommonRequest = constructListCommonRequest("containerId", containerId, "=");
        return list(CommonRequestModel.buildRequest(listCommonRequest));
    }

    @Override
    public ResponseEntity<?> V1PackingCreateAndUpdate(CommonRequestModel commonRequestModel, boolean checkForSync) throws Exception {
        PackingRequestV2 packingRequestV2 = (PackingRequestV2) commonRequestModel.getData();
        try {
            if (checkForSync && !Objects.isNull(syncConfig.IS_REVERSE_SYNC_ACTIVE) && !syncConfig.IS_REVERSE_SYNC_ACTIVE) {
                return syncQueueService.saveSyncRequest(SyncingConstants.PACKAGES, StringUtility.convertToString(packingRequestV2.getGuid()), packingRequestV2);
            }
            Optional<Packing> existingPacking = packingDao.findByGuid(packingRequestV2.getGuid());
            Packing packing = modelMapper.map(packingRequestV2, Packing.class);
            if (existingPacking != null && existingPacking.isPresent()) {
                packing.setId(existingPacking.get().getId());
                packing.setConsolidationId(existingPacking.get().getConsolidationId());
                packing.setShipmentId(existingPacking.get().getShipmentId());
            } else {
                if (packingRequestV2.getShipmentGuid() != null) {
                    Optional<ShipmentDetails> shipmentDetails = shipmentDao.findByGuid(packingRequestV2.getShipmentGuid());
                    if (shipmentDetails.isPresent())
                        packing.setShipmentId(shipmentDetails.get().getId());
                }
                if (packingRequestV2.getConsolidationGuid() != null) {
                    Optional<ConsolidationDetails> consolidationDetails = consolidationDao.findByGuid(packingRequestV2.getConsolidationGuid());
                    if (consolidationDetails.isPresent())
                        packing.setConsolidationId(consolidationDetails.get().getId());
                }
            }
            packing = packingDao.save(packing);
            PackingResponse response = objectMapper.convertValue(packing, PackingResponse.class);
            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception ex) {
            String responseMsg = ex.getMessage() != null ? ex.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, ex);
            TransactionAspectSupport.currentTransactionStatus().setRollbackOnly();
            throw new RuntimeException(ex);
        }
    }

    @Override
    public ResponseEntity<?> V1BulkPackingCreateAndUpdate(CommonRequestModel commonRequestModel) {
        BulkPackingRequestV2 bulkContainerRequest = (BulkPackingRequestV2) commonRequestModel.getData();
        try {
            List<ResponseEntity<?>> responses = new ArrayList<>();
            for (PackingRequestV2 containerRequest : bulkContainerRequest.getBulkPacking())
                responses.add(this.V1PackingCreateAndUpdate(CommonRequestModel.builder()
                        .data(containerRequest)
                        .build(), true));
            return ResponseHelper.buildSuccessResponse(responses);
        } catch (Exception e) {
            String responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            TransactionAspectSupport.currentTransactionStatus().setRollbackOnly();
            throw new RuntimeException(e);
        }
    }

    private static Containers addWeightVolume(PackingRequest request, Containers newContainer) throws Exception {
        BigDecimal finalWeight = request.getWeight().add(newContainer.getAchievedWeight());
        BigDecimal finalVolume = request.getVolume().add(newContainer.getAchievedVolume());
        finalWeight = new BigDecimal(convertUnit(Constants.MASS, finalWeight, newContainer.getAchievedWeightUnit(), request.getWeightUnit()).toString());
        finalVolume = new BigDecimal(UnitConversionUtility.convertUnit(Constants.VOLUME, finalVolume, newContainer.getAchievedVolumeUnit(), request.getVolumeUnit()).toString());
        newContainer.setAchievedWeight(finalWeight);
        newContainer.setAchievedVolume(finalVolume);
        return newContainer;
    }

    private static void subtractWeightVolume(BigDecimal finalWeight, Containers newContainer, Packing packing, BigDecimal finalVolume, PackingRequest request) throws Exception {
        finalWeight = newContainer.getAchievedWeight().subtract(packing.getWeight());
        finalVolume = newContainer.getAchievedVolume().subtract(packing.getVolume());
        finalWeight = new BigDecimal(convertUnit(Constants.MASS, finalWeight, newContainer.getAchievedWeightUnit(), request.getWeightUnit()).toString());
        finalVolume = new BigDecimal(UnitConversionUtility.convertUnit(Constants.VOLUME, finalVolume, newContainer.getAchievedVolumeUnit(), request.getVolumeUnit()).toString());

        newContainer.setAchievedWeight(finalWeight);
        newContainer.setAchievedVolume(finalVolume);
    }

    private IRunnerResponse convertEntityToDto(Packing packing) {
        return jsonHelper.convertValue(packing, PackingResponse.class);
    }

    private IRunnerResponse convertEntityToDto(Containers packing) {
        return jsonHelper.convertValue(packing, ContainerResponse.class);
    }

    private Packing convertRequestToEntity(PackingRequest request) {
        return jsonHelper.convertValue(request, Packing.class);
    }

    private List<IRunnerResponse> convertEntityListToDtoList(List<Packing> lst) {
        List<IRunnerResponse> responseList = new ArrayList<>();
        lst.forEach(packing -> {
            responseList.add(convertEntityToDto(packing));
        });
        return responseList;
    }

}
