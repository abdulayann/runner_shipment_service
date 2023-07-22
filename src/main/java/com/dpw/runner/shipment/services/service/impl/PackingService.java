package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.constants.PackingConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.IPackingDao;
import com.dpw.runner.shipment.services.dto.request.PackingRequest;
import com.dpw.runner.shipment.services.dto.response.PackingResponse;
import com.dpw.runner.shipment.services.entity.Packing;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IPackingService;
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
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;

@Slf4j
@Service
public class PackingService implements IPackingService {
    @Autowired
    IPackingDao packingDao;
    @Autowired
    ModelMapper modelMapper;

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
    public void uploadPacking(MultipartFile file) throws Exception {
        List<Packing> packingList = parseCSVFile(file);
        packingDao.saveAll(packingList);
    }

    @Override
    public void downloadPacking(HttpServletResponse response) throws Exception {
        List<Packing> packingList = packingDao.getAllPackings(); // Retrieve all packings from the database

        response.setContentType("text/csv");
        response.setHeader("Content-Disposition", "attachment; filename=\"packings.csv\"");

        try (PrintWriter writer = response.getWriter()) {
            writer.println(generateCSVHeader());
            for (Packing packing : packingList) {
                writer.println(formatContainerAsCSVLine(packing));
            }
        }
    }

    private String generateCSVHeader() {
        StringBuilder headerBuilder = new StringBuilder();
        Field[] fields = Packing.class.getDeclaredFields();
        for (Field field : fields) {
            if (headerBuilder.length() > 0) {
                headerBuilder.append(",");
            }
            headerBuilder.append(field.getName());
        }
        return headerBuilder.toString();
    }

    private String formatContainerAsCSVLine(Packing packing) {
        StringBuilder lineBuilder = new StringBuilder();
        Field[] fields = Packing.class.getDeclaredFields();
        for (Field field : fields) {
            field.setAccessible(true);
            try {
                Object value = field.get(packing);
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

    private List<Packing> parseCSVFile(MultipartFile file) throws IOException, CsvException {
        List<Packing> packingList = new ArrayList<>();
        try (CSVReader csvReader = new CSVReader(new InputStreamReader(file.getInputStream()))) {
            String[] header = csvReader.readNext();
            log.info("PARSED HEADER : " + Arrays.asList(header).toString());
            List<String[]> records = csvReader.readAll();
            for (String[] record : records) {
                Packing packings = new Packing();
                for (int i = 0; i < record.length; i++) {
                    setField(packings, header[i], record[i]);
                }
                packingList.add(packings);
            }
        } catch (NoSuchFieldException | IllegalAccessException e) {
            log.error(e.getMessage());
            throw new RuntimeException(e);
        }
        return packingList;
    }

    private void setField(Packing packings, String attributeName, String attributeValue) throws NoSuchFieldException, IllegalAccessException {
        Field field = packings.getClass().getDeclaredField(attributeName);
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
        } else if (fieldType == LocalDateTime.class) {
            parsedValue = LocalDateTime.parse(attributeValue);
        } else {
            throw new NoSuchFieldException();
        }

        field.set(packings, parsedValue);
        log.info("SAVING PACKING : " + packings.toString());
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
        try {
            packing = packingDao.save(packing);
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
            packingDao.delete(targetPacking.get());
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

    private IRunnerResponse convertEntityToDto(Packing packing) {
        return modelMapper.map(packing, PackingResponse.class);
    }

    private Packing convertRequestToEntity(PackingRequest request) {
        return modelMapper.map(request, Packing.class);
    }

    private List<IRunnerResponse> convertEntityListToDtoList(List<Packing> lst) {
        List<IRunnerResponse> responseList = new ArrayList<>();
        lst.forEach(packing -> {
            responseList.add(convertEntityToDto(packing));
        });
        return responseList;
    }

}
