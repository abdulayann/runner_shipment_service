package com.dpw.runner.shipment.services.document.util;

import org.apache.commons.codec.binary.Base64;
import org.springframework.web.multipart.MultipartFile;

import java.io.IOException;

public class FileUtils {
    public static String convertMultipartFileToBase64(MultipartFile file) throws IOException {
        byte[] bytes = file.getBytes();
        byte[] encodedBytes = Base64.encodeBase64(bytes);
        return new String(encodedBytes);
    }

    public static String getFileExtenation(String fileName) {
        int lastIndex = fileName.lastIndexOf(".");
        return lastIndex != -1 ? fileName.substring(lastIndex + 1) : "";
    }
}

