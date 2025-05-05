package com.dpw.runner.shipment.services.document.util;

import lombok.Data;
import lombok.Generated;
import lombok.Getter;
import org.apache.poi.ss.usermodel.Workbook;
import org.springframework.web.multipart.MultipartFile;

import java.io.*;

@Data
@Getter
@Generated
public class WorkbookMultipartFile implements MultipartFile {

  private final byte[] content;
  private final String name;
  private final String originalFilename;
  private final String contentType;

  public WorkbookMultipartFile(Workbook workbook, String originalFilename) throws IOException {
    this.name = "files";
    this.originalFilename = originalFilename;
    this.contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet";

    try (workbook; ByteArrayOutputStream bos = new ByteArrayOutputStream()) {
      workbook.write(bos);
      this.content = bos.toByteArray();
    }
  }

  @Override
  public String getName() {
    return name;
  }

  @Override
  public String getOriginalFilename() {
    return originalFilename;
  }

  @Override
  public String getContentType() {
    return contentType;
  }

  @Override
  public boolean isEmpty() {
    return content.length == 0;
  }

  @Override
  public long getSize() {
    return content.length;
  }

  @Override
  public byte[] getBytes() {
    return content;
  }

  @Override
  public InputStream getInputStream() {
    return new ByteArrayInputStream(content);
  }

  @Override
  public void transferTo(File dest) throws IOException {
    try (FileOutputStream fos = new FileOutputStream(dest)) {
      fos.write(content);
    }
  }
}

