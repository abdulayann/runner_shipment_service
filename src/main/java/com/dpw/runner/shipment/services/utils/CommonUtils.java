package com.dpw.runner.shipment.services.utils;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancy;
import com.dpw.runner.shipment.services.commons.requests.Criteria;
import com.dpw.runner.shipment.services.commons.requests.FilterCriteria;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.itextpdf.text.*;
import com.itextpdf.text.pdf.*;
import org.krysalis.barcode4j.impl.upcean.EAN13Bean;
import org.krysalis.barcode4j.output.bitmap.BitmapCanvasProvider;
import org.modelmapper.ModelMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.io.ByteArrayResource;
import org.springframework.stereotype.Component;
import org.springframework.transaction.TransactionSystemException;

import javax.imageio.ImageIO;
import java.awt.image.BufferedImage;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

@Component
public class CommonUtils {

    private static ObjectMapper mapper;

    @Autowired
    private ModelMapper modelMapper;

    @Autowired
    private JsonHelper jsonHelper;

    private static final Logger LOG = LoggerFactory.getLogger(CommonUtils.class);
    private static final String resourcePath = String.format("%s%s", System.getProperty("user.dir"), "/src/main/resources/");

    public CommonUtils(ObjectMapper mapper) {
        this.mapper = mapper;
    }

    public static FilterCriteria constructCriteria(String fieldName, Object value, String operator, String logicalOperator) {
        Criteria criteria = Criteria.builder().fieldName(fieldName).operator(operator).value(value).build();
        return FilterCriteria.builder().criteria(criteria).logicOperator(logicalOperator).build();
    }

    public static BufferedImage generateEAN13BarcodeImage(String barcodeText, int resolution) {
        EAN13Bean barcodeGenerator = new EAN13Bean();
        BitmapCanvasProvider canvas =
                new BitmapCanvasProvider(resolution, BufferedImage.TYPE_BYTE_BINARY, false, 0);

        barcodeGenerator.generateBarcode(canvas, barcodeText);
        return canvas.getBufferedImage();
    }

    public static ListCommonRequest constructListCommonRequest(String fieldName, Object value, String operator) {
        ListCommonRequest request = new ListCommonRequest();
        request.setPageNo(1);
        request.setPageSize(Integer.MAX_VALUE);


        List<FilterCriteria> criterias = new ArrayList<>();
        List<FilterCriteria> innerFilters = new ArrayList();
        Criteria criteria = Criteria.builder().fieldName(fieldName).operator(operator).value(value).build();
        FilterCriteria filterCriteria = FilterCriteria.builder().criteria(criteria).build();
        innerFilters.add(filterCriteria);
        criterias.add(FilterCriteria.builder().innerFilter(innerFilters).logicOperator(criterias.isEmpty() ? null : "or").build());
        request.setFilterCriteria(criterias);
        return request;
    }

    public static ListCommonRequest constructListRequestFromEntityId(Long entityId, String entityType) {
        FilterCriteria entityIdCriteria = FilterCriteria.builder()
                .innerFilter(Arrays.asList(FilterCriteria.builder()
                                .criteria(Criteria.builder()
                                        .fieldName("entityId")
                                        .operator("=")
                                        .value(entityId)
                                        .build()).build(),
                        FilterCriteria.builder()
                                .logicOperator("AND")
                                .criteria(Criteria.builder()
                                        .fieldName("entityType")
                                        .operator("=")
                                        .value(entityType)
                                        .build())
                                .build()))
                .build();

        ListCommonRequest listCommonRequest = ListCommonRequest.builder()
                .pageNo(1)
                .pageSize(Integer.MAX_VALUE)
                .filterCriteria(Arrays.asList(entityIdCriteria))
                .build();

        return listCommonRequest;
    }

    public static Criteria getFilterCriteria(String fieldName, Object value, String operator) {
        return Criteria.builder().fieldName(fieldName).operator(operator).value(value).build();
    }

    public static ListCommonRequest andCriteria(String fieldName, Object value, String operator, ListCommonRequest request) {
        if(request == null) {
            request = new ListCommonRequest();
            request.setPageNo(1);
            request.setPageSize(Integer.MAX_VALUE);
            request.setFilterCriteria(Arrays.asList(FilterCriteria.builder().innerFilter(new ArrayList<>()).build()));
        }

        List<FilterCriteria> criterias = request.getFilterCriteria();
        List<FilterCriteria> innerFilters = criterias.get(0).getInnerFilter();
        Criteria criteria = Criteria.builder().fieldName(fieldName).operator(operator).value(value).build();
        FilterCriteria filterCriteria = FilterCriteria.builder().criteria(criteria).build();
        if(innerFilters.size() > 0) {
            filterCriteria.setLogicOperator("and");
        }
        innerFilters.add(filterCriteria);
        return request;
    }

    public static ListCommonRequest orCriteria(String fieldName, Object value, String operator, ListCommonRequest request) {
        if(request == null) {
            request = new ListCommonRequest();
            request.setPageNo(1);
            request.setPageSize(Integer.MAX_VALUE);
            request.setFilterCriteria(Arrays.asList(FilterCriteria.builder().innerFilter(new ArrayList<>()).build()));
        }

        List<FilterCriteria> criterias = request.getFilterCriteria();
        List<FilterCriteria> innerFilters = criterias.get(0).getInnerFilter();
        Criteria criteria = Criteria.builder().fieldName(fieldName).operator(operator).value(value).build();
        FilterCriteria filterCriteria = FilterCriteria.builder().criteria(criteria).build();
        if(innerFilters.size() > 0) {
            filterCriteria.setLogicOperator("or");
        }
        innerFilters.add(filterCriteria);
        return request;
    }

    public static <T,P> P convertToClass(T obj, Class<P> clazz) {
        return mapper.convertValue(obj, clazz);
    }

    public static <T,P extends IRunnerResponse > List<P> convertToDtoList(final List<T> lst, Class<P> clazz) {
        return  lst.stream()
                .map(item -> convertToClass(item, clazz))
                .collect(Collectors.toList());
    }

    public static <T,P extends MultiTenancy> List<P> convertToEntityList(final List<T> lst, Class<P> clazz) {
        return  lst.stream()
                .map(item -> convertToClass(item, clazz))
                .collect(Collectors.toList());
    }

    public <T,P extends MultiTenancy> List<P> convertToCreateEntityList(final List<T> lst, Class<P> clazz) {
        return  lst.stream()
                .map(item -> this.convertToCreateClass(item, clazz))
                .collect(Collectors.toList());
    }

    public <T,P> List<P> convertToList(final List<T> lst, Class<P> clazz) {
        if(lst == null)
            return null;
        return  lst.stream()
                .map(item -> convertToClassModelMapper(item, clazz))
                .collect(Collectors.toList());
    }

    private  <T,P> P convertToClassModelMapper(T obj, Class<P> clazz) {
        return modelMapper.map(obj, clazz);
    }

    public <T,P> P convertToCreateClass(T obj, Class<P> clazz) {
        return jsonHelper.convertCreateValue(obj, clazz);
    }

    public static byte[] ImageToByte(BufferedImage img) throws IOException {
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        ImageIO.write(img, "jpg", baos);
        return baos.toByteArray();
    }

    public static boolean HasUnsupportedCharacters(String input) {
        int minSupportedAscii = 32;
        int maxSupportedAscii = 126;
        for (char c : input.toCharArray()) {
            if ((int) c < minSupportedAscii || (int) c > maxSupportedAscii) {
                return true;
            }
        }
        return false;
    }

    public static byte[] concatAndAddContent(List<byte[]> pdfByteContent) throws DocumentException, IOException {
        OutputStream ms = new ByteArrayOutputStream();
        Document doc = new Document();
        PdfCopy copy = new PdfSmartCopy(doc, ms);
        doc.open();
        PdfReader reader;
        for (byte[] dataByte : pdfByteContent) {
            reader = new PdfReader(dataByte);
            copy.addDocument(reader);
        }
        doc.close();
        return ((ByteArrayOutputStream)ms).toByteArray();
    }

    public static byte[] removeLastPage(byte[] bytes) throws IOException, DocumentException {
        PdfReader r = new PdfReader(bytes);
        OutputStream ms = new ByteArrayOutputStream();
        Document doc = new Document();
        PdfWriter w = PdfWriter.getInstance(doc, ms);
        doc.open();
        var pagesToKeep = r.getNumberOfPages();
        for(int page=1; page<pagesToKeep; page++){
            doc.newPage();
            w.getDirectContent().addTemplate(w.getImportedPage(r, page), 0, 0);
        }
        doc.close();
        return ((ByteArrayOutputStream)ms).toByteArray();
    }

    public static byte[] getLastPage(byte[] bytes) throws IOException, DocumentException {
        PdfReader r = new PdfReader(bytes);
        OutputStream ms = new ByteArrayOutputStream();
        Document doc = new Document();
        PdfWriter w = PdfWriter.getInstance(doc, ms);
        doc.open();
        doc.newPage();
        w.getDirectContent().addTemplate(w.getImportedPage(r, r.getNumberOfPages()), 0, 0);
        doc.close();
        return ((ByteArrayOutputStream)ms).toByteArray();
    }

    public static void AddWaterMark(PdfContentByte dc, String text, BaseFont font, float fontSize, float angle, BaseColor color, Rectangle realPageSize, Rectangle rect)
    {
        var gstate = new PdfGState();
        gstate.setFillOpacity(0.2f);
        gstate.setStrokeOpacity(0.3f);
        dc.saveState();
        dc.setGState(gstate);
        dc.setColorFill(color);
        dc.beginText();
        dc.setFontAndSize(font, fontSize);
        var ps = rect == null ? realPageSize : rect; /*dc.PdfDocument.PageSize is not always correct*/
        var x = (ps.getRight() + ps.getLeft()) / 2;
        var y = (ps.getBottom() + ps.getTop()) / 2;
        dc.showTextAligned(Element.ALIGN_CENTER, text, x, y, angle);
        dc.endText();
        dc.restoreState();
    }

    public static byte[] AddWatermark(byte[] bytes, BaseFont bf, String watermark) throws IOException, DocumentException {
        OutputStream ms = new ByteArrayOutputStream(10 * 1024);
        PdfReader reader = new PdfReader(bytes);
        PdfStamper stamper = new PdfStamper(reader, ms);
        int times = reader.getNumberOfPages();
        for (int i = 1; i <= times; i++)
        {
            var dc = stamper.getOverContent(i);
            AddWaterMark(dc, watermark, bf, 50, 35, new BaseColor(70, 70, 255), reader.getPageSizeWithRotation(i), null);
        }
        stamper.close();
        return ((ByteArrayOutputStream)ms).toByteArray();
    }

    public static ByteArrayResource getByteResource(InputStream inputStream, String fileName) throws IOException {
        return new ByteArrayResource(inputStream.readAllBytes()) {
            @Override
            public String getFilename() {
                return fileName;
            }
        };
    }

    public static double roundOffToTwoDecimalPlace(double number) {
        DecimalFormat decimalFormat = new DecimalFormat("#.##");
        return Double.parseDouble(decimalFormat.format(number));
    }

    public static String stringValueOf(Object o) {
        if(o == null)
            return null;
        return o.toString();
    }

    public static boolean IsStringNullOrEmpty(String s){
        return s == null || s.isEmpty();
    }

    public static String getErrorResponseMessage(Exception e, Class<?> clazz) {
        String responseMessage = "";
    responseMessage =
        switch (e.getClass().getSimpleName()) {
          case "TransactionSystemException" -> Objects.requireNonNull(
                  ((TransactionSystemException) e).getRootCause())
              .getMessage();
          default -> e.getMessage();
        };
        return responseMessage;
    }
}
