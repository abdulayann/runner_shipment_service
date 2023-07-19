package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.IContainerDao;
import com.dpw.runner.shipment.services.dao.interfaces.IEventDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentsContainersMappingDao;
import com.dpw.runner.shipment.services.dto.request.ContainerRequest;
import com.dpw.runner.shipment.services.dto.request.EventsRequest;
import com.dpw.runner.shipment.services.dto.response.ContainerResponse;
import com.dpw.runner.shipment.services.dto.response.JobResponse;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.entity.Events;
import com.dpw.runner.shipment.services.entity.enums.ContainerStatus;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IContainerService;
import com.nimbusds.jose.util.Pair;
import com.opencsv.CSVReader;
import com.opencsv.exceptions.CsvException;
import lombok.extern.slf4j.Slf4j;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.http.ResponseEntity;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;

import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.lang.reflect.Field;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.*;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;
import static com.dpw.runner.shipment.services.utils.CommonUtils.convertToEntityList;

@Slf4j
@Service
public class ContainerService implements IContainerService {

    @Autowired
    IContainerDao containerDao;
    @Autowired
    ModelMapper modelMapper;
    @Autowired
    IShipmentsContainersMappingDao shipmentsContainersMappingDao;
    @Autowired
    IEventDao eventDao;

    @Transactional
    public ResponseEntity<?> create(CommonRequestModel commonRequestModel) {
        String responseMsg;
        ContainerRequest request = (ContainerRequest) commonRequestModel.getData();
        if (request == null) {
            log.debug("Request is empty for Container Create with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }
        Containers container = convertRequestToEntity(request);
        List<EventsRequest> eventsRequestList = request.getEventsList();
        try {
            container = containerDao.save(container);
            if(request.getShipmentIds() != null) {
                shipmentsContainersMappingDao.updateShipmentsMappings(container.getId(), request.getShipmentIds());
            }
            if(eventsRequestList != null){
                List<Events> events = eventDao.saveEntityFromOtherEntity(
                        convertToEntityList(eventsRequestList, Events.class), container.getId(), Constants.CONTAINER);
                container.setEventsList(events);
            }
            log.info("Container Details Saved Successfully for Id {} with Request Id {}", container.getId(), LoggerHelper.getRequestIdFromMDC());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
        return ResponseHelper.buildSuccessResponse(convertEntityToDto(container));
    }

    @Override
    public void uploadContainers(MultipartFile file) throws Exception {
        List<Containers> containersList = parseCSVFile(file);
        containerDao.saveAll(containersList);
    }

    @Override
    public void downloadContainers(HttpServletResponse response) throws Exception {
        List<Containers> containersList = containerDao.getAllContainers(); // Retrieve all containers from the database

        response.setContentType("text/csv");
        response.setHeader("Content-Disposition", "attachment; filename=\"containers.csv\"");

        try (PrintWriter writer = response.getWriter()) {
            writer.println(generateCSVHeader());
            for (Containers container : containersList) {
                writer.println(formatContainerAsCSVLine(container));
            }
        }
    }

    private String generateCSVHeader() {
        StringBuilder headerBuilder = new StringBuilder();
        Field[] fields = Containers.class.getDeclaredFields();
        for (Field field : fields) {
            if (headerBuilder.length() > 0) {
                headerBuilder.append(",");
            }
            headerBuilder.append(field.getName());
        }
        return headerBuilder.toString();
    }

    private String formatContainerAsCSVLine(Containers container) {
        StringBuilder lineBuilder = new StringBuilder();
        Field[] fields = Containers.class.getDeclaredFields();
        for (Field field : fields) {
            field.setAccessible(true);
            try {
                Object value = field.get(container);
                if (lineBuilder.length() > 0) {
                    lineBuilder.append(",");
                }
                lineBuilder.append(value != null ? value.toString() : "");
            } catch (IllegalAccessException e) {
                e.printStackTrace();
            }
        }
        return lineBuilder.toString();
    }

    private List<Containers> parseCSVFile(MultipartFile file) throws IOException, CsvException {
        List<Containers> containersList = new ArrayList<>();
        List<String> mandatoryColumns = List.of("ContainerNumber", "ContainerCount", "ContainerCode");
        try (CSVReader csvReader = new CSVReader(new InputStreamReader(file.getInputStream()))) {
            String[] header = csvReader.readNext();
            log.info("PARSED HEADER : " + Arrays.asList(header).toString());
            List<String[]> records = csvReader.readAll();
            for (String[] record : records) {
                Containers containers = new Containers();
                for (int i = 0; i < record.length; i++) {
                    setField(containers, header[i], record[i]);
                }
                containersList.add(containers);
            }
        } catch (NoSuchFieldException | IllegalAccessException e) {
            log.error(e.getMessage());
            throw new RuntimeException(e);
        }
        return containersList;
    }

    private void setField(Containers containers, String attributeName, String attributeValue) throws NoSuchFieldException, IllegalAccessException {
        Field field = containers.getClass().getDeclaredField(attributeName);
        field.setAccessible(true);

        Class<?> fieldType = field.getType();
        Object parsedValue = null;

        if (fieldType == int.class || fieldType == Integer.class) {
            parsedValue = Integer.parseInt(attributeValue);
        } else if (fieldType == String.class) {
            parsedValue = attributeValue;
        } else if (fieldType == Long.class || fieldType == long.class) {
            parsedValue = Long.parseLong(attributeValue);
        } else if (fieldType == boolean.class || fieldType == Boolean.class) {
            parsedValue = Boolean.parseBoolean(attributeValue);
        } else if (fieldType == BigDecimal.class) {
            parsedValue = BigDecimal.valueOf(Double.valueOf(attributeValue));
        } else if (fieldType == ContainerStatus.class) {
            parsedValue = ContainerStatus.valueOf(attributeValue);
        } else if (fieldType == LocalDateTime.class) {
            parsedValue = LocalDateTime.parse(attributeValue);
        } else {
            throw new NoSuchFieldException();
        }

        field.set(containers, parsedValue);
        log.info("SAVING CONTAINER : " + containers.toString());
    }

    @Transactional
    public ResponseEntity<?> update(CommonRequestModel commonRequestModel) {
        String responseMsg;
        ContainerRequest request = (ContainerRequest) commonRequestModel.getData();
        if (request == null) {
            log.debug("Request is empty for Container Update with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }

        if (request.getId() == null) {
            log.debug("Request Id is null for Container Update with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }
        long id = request.getId();
        Optional<Containers> oldEntity = containerDao.findById(id);
        if (!oldEntity.isPresent()) {
            log.debug("Container is null for Id {} with Request Id {}", request.getId(), LoggerHelper.getRequestIdFromMDC());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }

        Containers containers = convertRequestToEntity(request);
        List<EventsRequest> eventsRequestList = request.getEventsList();
        containers.setId(oldEntity.get().getId());
        try {
            containers = containerDao.save(containers);
            if(request.getShipmentIds() != null) {
                shipmentsContainersMappingDao.updateShipmentsMappings(containers.getId(), request.getShipmentIds());
            }
            if(eventsRequestList != null){
                List<Events> events = eventDao.saveEntityFromOtherEntity(
                        convertToEntityList(eventsRequestList, Events.class), containers.getId(), Constants.CONTAINER);
                containers.setEventsList(events);
            }
            log.info("Updated the container details for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
        return ResponseHelper.buildSuccessResponse(convertEntityToDto(containers));
    }

    public ResponseEntity<?> list(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
            if (request == null) {
                log.error("Request is empty for Containers List with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            // construct specifications for filter request
            Pair<Specification<Containers>, Pageable> tuple = fetchData(request, Containers.class);
            Page<Containers> containersPage = containerDao.findAll(tuple.getLeft(), tuple.getRight());
            log.info("Container detail list retrieved successfully for Request Id {} ", LoggerHelper.getRequestIdFromMDC());
            return ResponseHelper.buildListSuccessResponse(
                    convertEntityListToDtoList(containersPage.getContent()),
                    containersPage.getTotalPages(),
                    containersPage.getTotalElements());
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
                log.error("Request is empty for Containers async list with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            // construct specifications for filter request
            Pair<Specification<Containers>, Pageable> tuple = fetchData(request, Containers.class);
            Page<Containers> containersPage = containerDao.findAll(tuple.getLeft(), tuple.getRight());
            log.info("Container detail async list retrieved successfully for Request Id {} ", LoggerHelper.getRequestIdFromMDC());
            return CompletableFuture.completedFuture(
                    ResponseHelper
                            .buildListSuccessResponse(
                                    convertEntityListToDtoList(containersPage.getContent()),
                                    containersPage.getTotalPages(),
                                    containersPage.getTotalElements()));
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
            log.debug("Request is empty for Containers delete with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }
        if (commonRequestModel.getId() == null) {
            log.debug("Request Id is null for Containers delete with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }
        Long id = commonRequestModel.getId();
        Optional<Containers> container = containerDao.findById(id);
        if (container.isEmpty()) {
            log.debug("Container details are null for id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
            return ResponseHelper.buildFailedResponse(Constants.NO_DATA);
        }
        try {
            containerDao.delete(container.get());
            log.info("Deleted container for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
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
                log.error("Request is empty for Container retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            if (request.getId() == null) {
                log.error("Request Id is null for Container retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            long id = request.getId();
            Optional<Containers> container = containerDao.findById(id);
            if (container.isEmpty()) {
                log.debug("Container is null for Id {} with Request Id {}", request.getId(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            log.info("Container detail fetched successfully for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
            JobResponse response = (JobResponse) convertEntityToDto(container.get());
            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    private IRunnerResponse convertEntityToDto(Containers container) {
        return modelMapper.map(container, ContainerResponse.class);
    }

    private Containers convertRequestToEntity(ContainerRequest request) {
        return modelMapper.map(request, Containers.class);
    }

    private List<IRunnerResponse> convertEntityListToDtoList(List<Containers> lst) {
        List<IRunnerResponse> responseList = new ArrayList<>();
        lst.forEach(containers -> {
            responseList.add(convertEntityToDto(containers));
        });
        return responseList;
    }

}
